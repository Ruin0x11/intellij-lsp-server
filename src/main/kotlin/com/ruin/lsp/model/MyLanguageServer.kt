package com.ruin.lsp.model

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.application.runUndoTransparentWriteAction
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
import com.ruin.lsp.commands.project.runconfigurations.RunConfigurationsCommand
import com.ruin.lsp.commands.project.runconfigurations.RunProjectCommand
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

    var client: MyLanguageClient? = null
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
        this.client = client as MyLanguageClient
    }

    fun computeAllDiagnostics() {
        workspace.managedTextDocuments.keys.forEach { computeDiagnostics(it) }
    }

    fun computeDiagnostics(uri: DocumentUri) {
        if (client == null) {
            return
        }

        val (doc, file) = invokeAndWaitIfNeeded( Computable<Pair<Document, PsiFile>?> {
            val file = resolvePsiFromUri(context.rootProject!!, uri) ?: return@Computable null
            val doc = getDocument(file) ?: return@Computable null
            Pair(doc, file)
        }) ?: return

        diagnosticsFutures[uri]?.cancel(true)

        diagnosticsFutures[uri] = ApplicationManager.getApplication()
            .executeOnPooledThread(DiagnosticsThread(file, doc, client!!))
    }

    override fun getTextDocumentService() = myTextDocumentService

    override fun getWorkspaceService() = myWorkspaceService


    // LSP protocol extensions for IDEA-specific features

    override fun implementations(params: TextDocumentPositionParams): CompletableFuture<MutableList<Location>> =
        asInvokeAndWaitFuture(context.rootProject!!, params.textDocument.uri, FindImplementationCommand(params.position), client)

    override fun runConfigurations(params: TextDocumentPositionParams): CompletableFuture<MutableList<RunConfigurationDescription>> =
        asInvokeAndWaitFuture(context.rootProject!!, params.textDocument.uri, RunConfigurationsCommand())

    override fun runProject(params: RunProjectParams): CompletableFuture<RunProjectCommandLine> =
        asInvokeAndWaitFuture(context.rootProject!!, params.textDocument.uri, RunProjectCommand(params.id))

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
    client: LanguageClient? = null,
    server: LanguageServer? = null): CompletableFuture<T> =
     CompletableFuture.supplyAsync {
        executeAndGetResult(project, uri, command, client, server)
    }

fun <T: Any> asCancellableInvokeAndWaitFuture(
    project: Project,
    uri: DocumentUri,
    command: DocumentCommand<T>,
    client: LanguageClient? = null,
    server: LanguageServer? = null): CompletableFuture<T> =
    CompletableFutures.computeAsync { cancelToken ->
        executeAndGetResult(project, uri, command, client, server, cancelToken)
    }

private val LOG = Logger.getInstance(MyLanguageServer::class.java)

private fun <T : Any> executeAndGetResult(
    project: Project,
    uri: DocumentUri,
    command: DocumentCommand<T>,
    client: LanguageClient? = null,
    server: LanguageServer? = null,
    cancelToken: CancelChecker? = null): T {
    return invokeAndWaitIfNeeded(Computable<T> {
        val file = ensurePsiFromUri(project, uri)
        val profiler = if (client != null) startProfiler(client) else DUMMY
        val context = ExecutionContext(project, file, client, server, profiler, cancelToken)
        profiler.finish("Done")
        val result = command.execute(context)
        command.dispose()
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
        codeLensProvider = CodeLensOptions(false)
        documentFormattingProvider = true
        documentRangeFormattingProvider = true
        documentOnTypeFormattingProvider = null
        renameProvider = false
        documentLinkProvider = null
        executeCommandProvider = null
        experimental = null
    }
