package com.ruin.lsp.commands.find

import com.github.kittinunf.result.Result
import com.intellij.codeInsight.TargetElementUtil
import com.intellij.psi.PsiElement
import com.ruin.lsp.commands.Command
import com.ruin.lsp.model.positionToOffset
import com.intellij.codeInsight.navigation.ImplementationSearcher
import com.intellij.openapi.application.ReadAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Ref
import com.intellij.psi.PsiFile
import com.ruin.lsp.util.*
import com.ruin.lsp.commands.errorResult
import com.ruin.lsp.model.LanguageServerException
import org.eclipse.lsp4j.*
import java.util.concurrent.CompletableFuture


class FindImplementationCommand(val textDocumentIdentifier: TextDocumentIdentifier,
                                val position: Position) : Command<MutableList<Location>> {
    override fun execute(project: Project, file: PsiFile): CompletableFuture<MutableList<Location>> {
        return CompletableFuture.supplyAsync {
            val doc = getDocument(file)
                ?: throw LanguageServerException("No document found.")

            val offset = positionToOffset(doc, position)
            val ref: Ref<Array<PsiElement>?> = Ref()
            withEditor(this, file, position) { editor ->
                val element = ensureTargetElement(editor)
                ref.set(searchImplementations(editor, element, offset))
            }
            val implementations = ref.get()

            implementations?.map(::elementToLocation)?.toMutableList() ?: mutableListOf()
        }
    }
}

fun searchImplementations(editor: Editor, element: PsiElement?, offset: Int): Array<PsiElement>? {
    val targetElementUtil = TargetElementUtil.getInstance()
    val onRef = ReadAction.compute<Boolean, RuntimeException> {
        targetElementUtil.findTargetElement(editor,
            getFlags() and (TargetElementUtil.REFERENCED_ELEMENT_ACCEPTED or TargetElementUtil.LOOKUP_ITEM_ACCEPTED).inv(),
            offset) == null
    }
    val shouldIncludeSelf = ReadAction.compute<Boolean, RuntimeException> {
        element == null || targetElementUtil.includeSelfInGotoImplementation(element)
    }
    val includeSelf = onRef && shouldIncludeSelf
    return ImplementationSearcher().searchImplementations(element, editor, includeSelf, onRef)
}

fun getFlags() = TargetElementUtil.getInstance().definitionSearchFlags;
