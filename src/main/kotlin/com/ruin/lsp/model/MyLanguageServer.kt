package com.ruin.lsp.model

import org.eclipse.lsp4j.InitializeParams
import org.eclipse.lsp4j.InitializeResult
import org.eclipse.lsp4j.services.LanguageServer
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.services.WorkspaceService
import java.util.concurrent.CompletableFuture

class MyLanguageServer : LanguageServer {
    var context = Context()

    var myTextDocumentService = MyTextDocumentService()
    var myWorkspaceService = MyWorkspaceService()

    override fun initialize(params: InitializeParams): CompletableFuture<InitializeResult> {
        return CompletableFuture.supplyAsync {
            context.clientCapabilities = params.capabilities
            InitializeResult(defaultServerCapabilities())
        }
    }

    override fun shutdown(): CompletableFuture<Any> {
        workspace().onShutdown()

        return CompletableFuture.completedFuture(Unit)
    }

    override fun exit() {

    }

    override fun getTextDocumentService() = myTextDocumentService

    override fun getWorkspaceService() = myWorkspaceService
}
