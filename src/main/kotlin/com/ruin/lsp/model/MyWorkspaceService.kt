package com.ruin.lsp.model

import com.google.gson.JsonObject
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
        val settings = params.settings as JsonObject
        val intellij = settings.get("intellij")?.asJsonObject ?: return
        val options = intellij.entrySet().map {
            Pair(it.key, it.value.asString)
        }
        server.context.config.putAll(options)
    }

    override fun symbol(params: WorkspaceSymbolParams): CompletableFuture<MutableList<out SymbolInformation>> {
        val cachedProjectPath = sProjectCache.keys.firstOrNull()
            ?: return CompletableFuture.supplyAsync { mutableListOf<SymbolInformation>() }
        return server.asInvokeAndWaitFuture(server.context.rootProject!!, getURIForFile(File(cachedProjectPath)), WorkspaceSymbolCommand(params.query))
    }
}
