package com.ruin.lsp.commands

import com.intellij.openapi.Disposable
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile
import com.ruin.lsp.model.LanguageServerException
import com.ruin.lsp.util.Profiler
import com.ruin.lsp.util.getURIForFile
import com.ruin.lsp.values.DocumentUri
import org.eclipse.lsp4j.jsonrpc.CancelChecker
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.LanguageServer
import java.util.concurrent.CompletableFuture

interface Command<out T: Any?>: Disposable {
    override fun dispose() {}

    fun execute(ctx: ExecutionContext): T
}

fun errorResult(message: String) = CompletableFuture.supplyAsync { throw LanguageServerException(message) }

data class ExecutionContext(val project: Project,
                            val file: PsiFile,
                            val client: LanguageClient? = null,
                            val server: LanguageServer? = null,
                            val profiler: Profiler? = null,
                            val cancelToken: CancelChecker? = null) {
    val uri: DocumentUri
        get() = getURIForFile(file)
}
