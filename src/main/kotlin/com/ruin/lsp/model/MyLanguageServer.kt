package com.ruin.lsp.model

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.ServiceManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Document
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Computable
import com.intellij.psi.PsiFile
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.commands.ProjectCommand
import com.ruin.lsp.commands.document.diagnostics.DiagnosticsThread
import com.ruin.lsp.commands.document.find.FindImplementationCommand
import com.ruin.lsp.commands.project.dialog.OpenProjectStructureCommand
import com.ruin.lsp.commands.project.dialog.ToggleFrameVisibilityCommand
import com.ruin.lsp.util.*
import com.ruin.lsp.values.DocumentUri
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.CancelChecker
import org.eclipse.lsp4j.jsonrpc.CompletableFutures
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.LanguageClientAware
import org.eclipse.lsp4j.services.LanguageServer
import java.util.concurrent.CompletableFuture
import java.util.concurrent.Future

class MyLanguageServer : LanguageServer, MyLanguageServerExtensions, LanguageClientAware {
    private val LOG = Logger.getInstance(MyLanguageServer::class.java)
    var context = Context()

    val workspace: WorkspaceManager by lazy { ServiceManager.getService<WorkspaceManager>(WorkspaceManager::class.java)!! }

    var myTextDocumentService = MyTextDocumentService(this)
    var myWorkspaceService = MyWorkspaceService(this)

    var diagnosticsFutures: HashMap<DocumentUri, Future<*>> = HashMap()

    override fun initialize(params: InitializeParams): CompletableFuture<InitializeResult> {
        return CompletableFuture.supplyAsync {
            context.clientCapabilities = params.capabilities

            ApplicationManager.getApplication().invokeAndWait {
                context.rootProject = resolveProjectFromRootUri(params.rootUri)
            }

            LOG.info("LSP was initialized. Project: ${context.rootProject}")

            InitializeResult(defaultServerCapabilities())
        }
    }

    override fun shutdown(): CompletableFuture<Any> {
        workspace.onShutdown()

        return CompletableFuture.completedFuture(Unit)
    }

    override fun exit() {

    }

    override fun connect(client: LanguageClient?) {
        this.context.client = client as MyLanguageClient
    }

    fun computeAllDiagnostics() {
        workspace.managedTextDocuments.keys.forEach { computeDiagnostics(it) }
    }

    fun computeDiagnostics(uri: DocumentUri) {
        if (this.context.client == null) {
            return
        }

        val (doc, file) = invokeAndWaitIfNeeded( Computable<Pair<Document, PsiFile>?> {
            val file = resolvePsiFromUri(context.rootProject!!, uri) ?: return@Computable null
            val doc = getDocument(file) ?: return@Computable null
            Pair(doc, file)
        }) ?: return

        diagnosticsFutures[uri]?.cancel(true)

        diagnosticsFutures[uri] = ApplicationManager.getApplication()
            .executeOnPooledThread(DiagnosticsThread(file, doc, this.context.client!!))
    }

    override fun getTextDocumentService() = myTextDocumentService

    override fun getWorkspaceService() = myWorkspaceService


    // LSP protocol extensions for IDEA-specific features

    override fun implementations(params: TextDocumentPositionParams): CompletableFuture<MutableList<Location>> =
        asInvokeAndWaitFuture(context.rootProject!!, params.textDocument.uri, FindImplementationCommand(params.position), this)

    override fun openProjectStructure(params: TextDocumentPositionParams): CompletableFuture<Boolean> =
        asInvokeAndWaitFuture(context.rootProject!!, params.textDocument.uri, OpenProjectStructureCommand())

    override fun toggleFrameVisibility(params: TextDocumentPositionParams): CompletableFuture<Boolean> =
        asInvokeAndWaitFuture(context.rootProject!!, params.textDocument.uri, ToggleFrameVisibilityCommand())
}


fun <T: Any> asInvokeAndWaitFuture(
    project: Project,
    uri: DocumentUri,
    command: ProjectCommand<T>): CompletableFuture<T> =
    CompletableFuture.supplyAsync {
        invokeAndWaitIfNeeded(Computable<T> {
            command.execute(project)
        })
    }

fun <T: Any> asInvokeAndWaitFuture(
    project: Project,
    uri: DocumentUri,
    command: DocumentCommand<T>,
    server: MyLanguageServer): CompletableFuture<T> =
     CompletableFuture.supplyAsync {
        executeAndGetResult(project, uri, command, server.context, server)
    }

fun <T: Any> asCancellableInvokeAndWaitFuture(
    project: Project,
    uri: DocumentUri,
    command: DocumentCommand<T>,
    server: MyLanguageServer): CompletableFuture<T> =
    CompletableFutures.computeAsync { cancelToken ->
        executeAndGetResult(project, uri, command, server.context, server, cancelToken)
    }

private val LOG = Logger.getInstance(MyLanguageServer::class.java)

private fun <T : Any> executeAndGetResult(
    project: Project,
    uri: DocumentUri,
    command: DocumentCommand<T>,
    context: Context = Context(),
    server: LanguageServer? = null,
    cancelToken: CancelChecker? = null): T {
    return invokeAndWaitIfNeeded(Computable<T> {
        val tempDir = context.config["temporaryDirectory"]
        val file = ensurePsiFromUri(project, uri, tempDir)
        val profiler = if (context.client != null) startProfiler(context.client!!) else DUMMY
        profiler.mark("Start ${command.javaClass.canonicalName}")
        val executionContext = ExecutionContext(project, file, context.client, server, profiler, cancelToken)
        val result = command.execute(executionContext)
        command.dispose()
        profiler.finish("Done ${command.javaClass.canonicalName}")
        result
    })
}

fun <T: Any> invokeCommandAndWait(command: com.ruin.lsp.commands.DocumentCommand<T>,
                                  project: Project,
                                  file: PsiFile,
                                  client: LanguageClient? = null): T {
    val context = ExecutionContext(project, file, client)

    val result = invokeAndWaitIfNeeded(Computable {
        command.execute(context)
    })

    command.dispose()
    return result
}

fun <T: Any> invokeCommandAndWait(command: com.ruin.lsp.commands.ProjectCommand<T>,
                                  project: Project): T {
    val result = invokeAndWaitIfNeeded(Computable {
        command.execute(project)
    })

    command.dispose()
    return result
}

fun defaultServerCapabilities() =
     ServerCapabilities().apply {
        textDocumentSync = Either.forRight(TextDocumentSyncOptions().apply {
            openClose = true
            this.change = TextDocumentSyncKind.Incremental
            save = SaveOptions(true)
        })
        hoverProvider = true
        completionProvider = CompletionOptions(true, listOf(".", "@", "#"))
        signatureHelpProvider = null
        definitionProvider = true
        referencesProvider = true
        documentHighlightProvider = true
        documentSymbolProvider = true
        workspaceSymbolProvider = true
        codeActionProvider = false
        codeLensProvider = null
        documentFormattingProvider = true
        documentRangeFormattingProvider = true
        documentOnTypeFormattingProvider = null
        renameProvider = false
        documentLinkProvider = null
        executeCommandProvider = null
        experimental = null
    }
