package com.ruin.lsp.model

import com.ruin.lsp.commands.project.symbol.SymbolCommand
import com.ruin.lsp.util.getURIForFile
import com.ruin.lsp.util.sProjectCache
import org.eclipse.lsp4j.DidChangeConfigurationParams
import org.eclipse.lsp4j.DidChangeWatchedFilesParams
import org.eclipse.lsp4j.SymbolInformation
import org.eclipse.lsp4j.WorkspaceSymbolParams
import org.eclipse.lsp4j.services.WorkspaceService
import java.io.File
import java.util.concurrent.CompletableFuture

class MyWorkspaceService(val context: MyLanguageServer) : WorkspaceService {
    override fun didChangeWatchedFiles(params: DidChangeWatchedFilesParams) {
    }

    override fun didChangeConfiguration(params: DidChangeConfigurationParams) {
    }

    override fun symbol(params: WorkspaceSymbolParams): CompletableFuture<MutableList<out SymbolInformation>> {
        val cachedProjectPath = sProjectCache.keys.firstOrNull()
            ?: return CompletableFuture.supplyAsync { mutableListOf<SymbolInformation>() }
        return asInvokeAndWaitFuture(getURIForFile(File(cachedProjectPath)), SymbolCommand(params.query))
    }
}
