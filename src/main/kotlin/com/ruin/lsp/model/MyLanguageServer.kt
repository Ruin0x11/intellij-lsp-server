package com.ruin.lsp.model

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.ServiceManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.util.Computable
import com.intellij.openapi.util.Ref
import com.ruin.lsp.commands.Command
import com.ruin.lsp.util.ensurePsiFromUri
import com.ruin.lsp.util.invokeAndWaitIfNeeded
import com.ruin.lsp.values.DocumentUri
import org.eclipse.lsp4j.CompletionOptions
import org.eclipse.lsp4j.InitializeParams
import org.eclipse.lsp4j.InitializeResult
import org.eclipse.lsp4j.ServerCapabilities
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.LanguageClientAware
import org.eclipse.lsp4j.services.LanguageServer
import java.util.concurrent.CompletableFuture

class MyLanguageServer : LanguageServer {
    private val LOG = Logger.getInstance(MyLanguageServer::class.java)
    var context = Context()

    val workspace: WorkspaceManager by lazy { ServiceManager.getService<WorkspaceManager>(WorkspaceManager::class.java)!! }

    var myTextDocumentService = MyTextDocumentService(context)
    var myWorkspaceService = MyWorkspaceService(context)


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

    override fun getTextDocumentService() = myTextDocumentService

    override fun getWorkspaceService() = myWorkspaceService
}

fun <T: Any>asInvokeAndWaitFuture(uri: DocumentUri, command: Command<T>): CompletableFuture<T> {
    return CompletableFuture.supplyAsync {
        invokeAndWaitIfNeeded(Computable<T> {
            val (project, file) = ensurePsiFromUri(uri)
            command.execute(project, file)
        })
    }
}

fun <T: Any> invokeCommandAndWait(command: com.ruin.lsp.commands.Command<T>, uri: DocumentUri): T {
    val ref: Ref<T> = Ref()
        val (project, file) = ensurePsiFromUri(uri)

        val result = invokeAndWaitIfNeeded( Computable { command.execute(project, file) } )

        command.dispose()
        ref.set(result)
    return ref.get()
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

