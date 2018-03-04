package com.ruin.lsp.commands.symbol

import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.PsiRecursiveElementVisitor
import java.lang.UnsupportedOperationException

internal class DocumentSymbolPsiVisitor(
    private val psiFile: PsiFile,
    private val onElement: (PsiElement) -> Unit
) : PsiRecursiveElementVisitor() {

    fun visit() {
        visitElement(psiFile)
    }

    override fun visitElement(element: PsiElement) {
        onElement(element)
        super.visitElement(element)
    }

    override fun visitFile(file: PsiFile?) {
        throw UnsupportedOperationException("Use visit() instead.")
    }
}

