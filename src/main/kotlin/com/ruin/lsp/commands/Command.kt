package com.ruin.lsp.commands

import com.github.kittinunf.result.Result
import com.intellij.openapi.Disposable
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile
import com.ruin.lsp.model.LanguageServerException
import java.util.concurrent.CompletableFuture

interface Command<T: Any>: Disposable {
    override fun dispose() {}

    fun execute(project: Project, file: PsiFile): CompletableFuture<T>
}

fun errorResult(message: String) = CompletableFuture.supplyAsync { throw LanguageServerException(message) }
