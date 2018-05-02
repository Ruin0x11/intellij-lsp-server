package com.ruin.lsp.model

import com.google.gson.JsonObject
import com.ruin.lsp.commands.ProjectCommand
import com.ruin.lsp.commands.project.symbol.WorkspaceSymbolCommand
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.services.WorkspaceService
import java.util.concurrent.CompletableFuture

class MyWorkspaceService(val server: MyLanguageServer) : WorkspaceService {
    override fun didChangeWatchedFiles(params: DidChangeWatchedFilesParams) {}

    override fun didChangeWorkspaceFolders(params: DidChangeWorkspaceFoldersParams) {}

    override fun didChangeConfiguration(params: DidChangeConfigurationParams) {
        val settings = params.settings as JsonObject
        val intellij = settings.get("intellij")?.asJsonObject ?: return
        val options = intellij.entrySet().map {
            Pair(it.key, it.value.asString)
        }
        server.context.config.putAll(options)
    }

    override fun symbol(params: WorkspaceSymbolParams): CompletableFuture<MutableList<out SymbolInformation>> =
        server.asInvokeAndWaitFuture(server.context.rootProject!!, WorkspaceSymbolCommand(params.query))

    override fun executeCommand(params: ExecuteCommandParams): CompletableFuture<Any> {
        val command = ProjectCommand.from(params.command) ?: return CompletableFuture.supplyAsync { false }
        return server.asInvokeAndWaitFuture(server.context.rootProject!!, command)
    }
}
