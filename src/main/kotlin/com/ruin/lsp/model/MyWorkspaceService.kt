package com.ruin.lsp.model

import com.intellij.openapi.components.ServiceManager
import org.eclipse.lsp4j.DidChangeConfigurationParams
import org.eclipse.lsp4j.DidChangeWatchedFilesParams
import org.eclipse.lsp4j.SymbolInformation
import org.eclipse.lsp4j.WorkspaceSymbolParams
import org.eclipse.lsp4j.services.WorkspaceService
import java.util.concurrent.CompletableFuture

class MyWorkspaceService(val context: Context) : WorkspaceService {
    val workspace: WorkspaceManager by lazy { ServiceManager.getService<WorkspaceManager>(WorkspaceManager::class.java)!! }

    override fun didChangeWatchedFiles(params: DidChangeWatchedFilesParams) {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun didChangeConfiguration(params: DidChangeConfigurationParams) {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun symbol(params: WorkspaceSymbolParams): CompletableFuture<MutableList<out SymbolInformation>> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }


}
