package com.ruin.lsp.model

import com.ruin.lsp.commands.project.symbol.WorkspaceSymbolCommand
import com.ruin.lsp.util.getURIForFile
import com.ruin.lsp.util.sProjectCache
import org.eclipse.lsp4j.DidChangeConfigurationParams
import org.eclipse.lsp4j.DidChangeWatchedFilesParams
import org.eclipse.lsp4j.SymbolInformation
import org.eclipse.lsp4j.WorkspaceSymbolParams
import org.eclipse.lsp4j.services.WorkspaceService
import java.io.File
import java.util.concurrent.CompletableFuture

class MyWorkspaceService(val server: MyLanguageServer) : WorkspaceService {
    override fun didChangeWatchedFiles(params: DidChangeWatchedFilesParams) {
    }

    override fun didChangeConfiguration(params: DidChangeConfigurationParams) {
    }

    override fun symbol(params: WorkspaceSymbolParams): CompletableFuture<MutableList<out SymbolInformation>> =
        asInvokeAndWaitFuture(server.context.rootProject!!, WorkspaceSymbolCommand(params.query))

}
