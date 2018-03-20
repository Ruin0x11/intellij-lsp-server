package com.ruin.lsp.commands.document.completion

import com.intellij.codeInsight.daemon.impl.quickfix.ImportClassFix
import com.intellij.psi.JavaPsiFacade
import com.intellij.psi.PsiClass
import com.intellij.psi.PsiJavaCodeReferenceElement
import com.intellij.psi.impl.PsiManagerEx
import com.intellij.psi.impl.source.PsiJavaCodeReferenceElementImpl
import com.intellij.psi.impl.source.tree.TreeElement
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.model.CompletionResolveIndex
import com.ruin.lsp.model.PreviousCompletionCacheService
import com.ruin.lsp.model.applyChange
import com.ruin.lsp.util.differenceFromAction
import org.eclipse.lsp4j.CompletionItem

class CompletionItemResolveCommand(val item: CompletionItem)  : DocumentCommand<CompletionItem> {
    override fun execute(ctx: ExecutionContext): CompletionItem {
        val completionCache = PreviousCompletionCacheService.getInstance()
        val lookupElement = completionCache.resolveItem(item.data as CompletionResolveIndex) ?: return item
        var newItem = item

        if(lookupElement.psiElement != null) {
            val importEdits = differenceFromAction(ctx.file) { editor, copy ->
                val elt = lookupElement.psiElement!!
                val r = JavaPsiFacade.getInstance(ctx.project).elementFactory

                if (elt is PsiClass) {
                    val ref = r.createReferenceExpression(elt)
                    ImportClassFix(ref).invoke(ctx.project, editor, copy)
                }
            }
            newItem.additionalTextEdits = importEdits
        }
        return newItem
    }
}
