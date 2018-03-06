package com.ruin.lsp.model

import com.intellij.openapi.components.ServiceManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.util.Computable
import com.ruin.lsp.commands.Command
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.commands.find.FindImplementationCommand
import com.ruin.lsp.util.DUMMY
import com.ruin.lsp.util.ensurePsiFromUri
import com.ruin.lsp.util.invokeAndWaitIfNeeded
import com.ruin.lsp.util.startProfiler
import com.ruin.lsp.values.DocumentUri
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.CancelChecker
import org.eclipse.lsp4j.jsonrpc.CompletableFutures
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.LanguageClientAware
import org.eclipse.lsp4j.services.LanguageServer
import java.util.concurrent.CompletableFuture

class MyLanguageServer : LanguageServer, MyLanguageServerExtensions, LanguageClientAware {
    private val LOG = Logger.getInstance(MyLanguageServer::class.java)
    var context = Context()

    val workspace: WorkspaceManager by lazy { ServiceManager.getService<WorkspaceManager>(WorkspaceManager::class.java)!! }

    var myTextDocumentService = MyTextDocumentService(this)
    var myWorkspaceService = MyWorkspaceService(this)

    var client: LanguageClient? = null

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

    override fun getTextDocumentService() = myTextDocumentService

    override fun getWorkspaceService() = myWorkspaceService

    override fun implementations(params: TextDocumentPositionParams): CompletableFuture<MutableList<Location>> =
        asInvokeAndWaitFuture(params.textDocument.uri, FindImplementationCommand(params.position), client)
}

fun <T: Any> asInvokeAndWaitFuture(
    uri: DocumentUri,
    command: Command<T>,
    client: LanguageClient?): CompletableFuture<T> =
     CompletableFuture.supplyAsync {
        executeAndGetResult(uri, client, command)
    }

fun <T: Any> asCancellableInvokeAndWaitFuture(
    uri: DocumentUri,
    command: Command<T>,
    client: LanguageClient?): CompletableFuture<T> =
    CompletableFutures.computeAsync { cancelToken ->
        executeAndGetResult(uri, client, command, cancelToken)
    }

private fun <T : Any> executeAndGetResult(
    uri: DocumentUri,
    client: LanguageClient?,
    command: Command<T>,
    cancelToken: CancelChecker? = null): T {
    return invokeAndWaitIfNeeded(Computable<T> {
        val (project, file) = ensurePsiFromUri(uri)
        val profiler = if (client != null) startProfiler(client) else DUMMY
        val context = ExecutionContext(project, file, client, profiler, cancelToken)
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

