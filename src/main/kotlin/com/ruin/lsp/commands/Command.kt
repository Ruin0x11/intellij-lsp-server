package com.ruin.lsp.commands

import com.github.kittinunf.result.Result
import com.intellij.openapi.Disposable
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile
import com.ruin.lsp.model.LanguageServerException

interface Command<out T: Any>: Disposable {
    override fun dispose() {

    }

    fun execute(project: Project, file: PsiFile): Result<T, Exception>
}

fun errorResult(message: String) = Result.error(LanguageServerException(message))
