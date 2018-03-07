package com.ruin.lsp.model

import com.intellij.openapi.Disposable
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.application.invokeAndWaitIfNeed
import com.intellij.openapi.application.runReadAction
import com.intellij.openapi.components.ServiceManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.util.Computable
import com.intellij.psi.PsiFile
import com.ruin.lsp.commands.Command
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.commands.diagnostics.DiagnosticsCommand
import com.ruin.lsp.commands.diagnostics.DiagnosticsThread
import com.ruin.lsp.commands.find.FindImplementationCommand
import com.ruin.lsp.util.*
import com.ruin.lsp.values.DocumentUri
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.CancelChecker
import org.eclipse.lsp4j.jsonrpc.CompletableFutures
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

    var client: LanguageClient? = null
    var diagnosticsFutures: HashMap<DocumentUri, Future<*>> = HashMap()

    override fun initialize(params: InitializeParams): CompletableFuture<InitializeResult> {
        return CompletableFuture.supplyAsync {
            context.clientCapabilities = params.capabilities

            LOG.info("LSP was initialized.")

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
        this.client = client
    }

    fun computeDiagnostics(uri: DocumentUri) {
        if (client == null) {
            return
        }

        val (doc, file) = invokeAndWaitIfNeeded( Computable<Pair<Document, PsiFile>?> {
            val (_, file) = resolvePsiFromUri(uri) ?: return@Computable null
            val doc = getDocument(file) ?: return@Computable null
            Pair(doc, file)
        }) ?: return

        diagnosticsFutures[uri]?.cancel(true)

        LOG.info("Computing diagnostics for $uri")

        diagnosticsFutures[uri] = ApplicationManager.getApplication()
            .executeOnPooledThread(DiagnosticsThread(file, doc, client!!))
    }

    override fun getTextDocumentService() = myTextDocumentService

    override fun getWorkspaceService() = myWorkspaceService

    override fun implementations(params: TextDocumentPositionParams): CompletableFuture<MutableList<Location>> =
        asInvokeAndWaitFuture(params.textDocument.uri, FindImplementationCommand(params.position), client)
}

fun <T: Any> asInvokeAndWaitFuture(
    uri: DocumentUri,
    command: Command<T>,
    client: LanguageClient? = null,
    server: LanguageServer? = null): CompletableFuture<T> =
     CompletableFuture.supplyAsync {
        executeAndGetResult(uri, command, client, server)
    }

fun <T: Any> asCancellableInvokeAndWaitFuture(
    uri: DocumentUri,
    command: Command<T>,
    client: LanguageClient? = null,
    server: LanguageServer? = null): CompletableFuture<T> =
    CompletableFutures.computeAsync { cancelToken ->
        executeAndGetResult(uri, command, client, server, cancelToken)
    }

private fun <T : Any> executeAndGetResult(
    uri: DocumentUri,
    command: Command<T>,
    client: LanguageClient? = null,
    server: LanguageServer? = null,
    cancelToken: CancelChecker? = null): T {
    return invokeAndWaitIfNeeded(Computable<T> {
        val (project, file) = ensurePsiFromUri(uri)
        val profiler = if (client != null) startProfiler(client) else DUMMY
        val context = ExecutionContext(project, file, client, server, profiler, cancelToken)
        profiler.finish("Done")
        val result = command.execute(context)
        command.dispose()
        result
    })
}


fun <T: Any> invokeCommandAndWait(command: com.ruin.lsp.commands.Command<T>,
                                  uri: DocumentUri,
                                  client: LanguageClient? = null): T {
    val (project, file) = ensurePsiFromUri(uri)
    val context = ExecutionContext(project, file, client)

    val result = invokeAndWaitIfNeeded(Computable {
        command.execute(context)
    })

    command.dispose()
    return result
}

fun defaultServerCapabilities() =
     ServerCapabilities().apply {
        textDocumentSync = null
        hoverProvider = true
        completionProvider = CompletionOptions(false, listOf(".", "@", "#"))
        signatureHelpProvider = null
        definitionProvider = true
        referencesProvider = true
        documentHighlightProvider = true
        documentSymbolProvider = true
        workspaceSymbolProvider = false
        codeActionProvider = false
        codeLensProvider = null
        documentFormattingProvider = false
        documentRangeFormattingProvider = false
        documentOnTypeFormattingProvider = null
        renameProvider = false
        documentLinkProvider = null
        executeCommandProvider = null
        experimental = null
    }
