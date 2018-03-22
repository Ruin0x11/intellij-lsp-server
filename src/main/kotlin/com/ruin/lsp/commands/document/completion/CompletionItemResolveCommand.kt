package com.ruin.lsp.commands.document.completion

import com.google.gson.JsonObject
import com.intellij.openapi.application.ApplicationManager
import com.intellij.psi.*
import com.intellij.psi.codeStyle.CodeStyleManager
import com.intellij.psi.impl.source.PsiImportListImpl
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.model.CompletionResolveIndex
import com.ruin.lsp.model.PreviousCompletionCacheService
import com.ruin.lsp.util.differenceFromAction
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.TextEdit

class CompletionItemResolveCommand(val item: CompletionItem)  : DocumentCommand<CompletionItem> {
    override fun execute(ctx: ExecutionContext): CompletionItem {
        val completionCache = PreviousCompletionCacheService.getInstance()
        val lookupElement =
            if(item.data is CompletionResolveIndex) {
                completionCache.resolveItem(item.data as CompletionResolveIndex)
            } else {
                completionCache.resolveItem(item.data as JsonObject)
            } ?: return item
        val newItem = item

        val elt = lookupElement.psiElement
        if (elt != null) {
            newItem.additionalTextEdits = autoImportIfNeededEdits(elt, ctx.file) ?: listOf()
        }
        return newItem
    }
}

private fun autoImportIfNeededEdits(elt: PsiElement, file: PsiFile): List<TextEdit>? {
    return differenceFromAction(file) { _, copy ->
        if (elt is PsiClass && copy is PsiJavaFile) {
            val manager = PsiDocumentManager.getInstance(copy.project)
            manager.commitDocument(copy.viewProvider.document!!)
            copy.importClass(elt)
            ApplicationManager.getApplication().runWriteAction {
                val codeStyleManager = CodeStyleManager.getInstance(copy.project)
                codeStyleManager.reformat(copy.importList as PsiImportListImpl)
            }
        }
    }
}
