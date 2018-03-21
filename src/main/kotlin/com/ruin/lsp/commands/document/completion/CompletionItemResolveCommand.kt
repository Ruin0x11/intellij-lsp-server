package com.ruin.lsp.commands.document.completion

import com.google.gson.JsonObject
import com.intellij.codeInsight.completion.CompletionUtilCore
import com.intellij.codeInsight.completion.JavaCompletionUtil
import com.intellij.codeInsight.daemon.impl.quickfix.ImportClassFix
import com.intellij.lang.java.JavaImportOptimizer
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.application.invokeAndWaitIfNeed
import com.intellij.openapi.application.runUndoTransparentWriteAction
import com.intellij.psi.*
import com.intellij.psi.codeStyle.CodeStyleManager
import com.intellij.psi.codeStyle.CodeStyleSettingsManager
import com.intellij.psi.codeStyle.JavaCodeStyleManager
import com.intellij.psi.formatter.java.JavaFormatterUtil
import com.intellij.psi.impl.PsiJavaParserFacadeImpl
import com.intellij.psi.impl.source.PsiImportListImpl
import com.intellij.psi.impl.source.codeStyle.ImportHelper
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.model.CompletionResolveIndex
import com.ruin.lsp.model.PreviousCompletionCacheService
import com.ruin.lsp.util.asWriteAction
import com.ruin.lsp.util.differenceFromAction
import com.ruin.lsp.util.reloadDocument
import org.eclipse.lsp4j.CompletionItem

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
            val importEdits = differenceFromAction(ctx.file) { editor, copy ->
                if (elt is PsiClass && copy is PsiJavaFile) {
                    //val ref = JavaPsiFacade.getInstance(ctx.project).parserFacade.createReferenceFromText(lookupElement.lookupString, elt)
                    //ImportClassFix(ref).doFix(editor, false, false)
                    val manager = PsiDocumentManager.getInstance(copy.project)
                    manager.commitDocument(copy.viewProvider.document!!)
                    copy.importClass(elt)
                    ApplicationManager.getApplication().runWriteAction {
                        val codeStyleManager = CodeStyleManager.getInstance(copy.project)
                        codeStyleManager.reformat(copy.importList as PsiImportListImpl)
                    }
                }
            }
            newItem.additionalTextEdits = importEdits ?: listOf()
        }
        return newItem
    }
}
