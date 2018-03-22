package com.ruin.lsp.commands.document.find

import com.intellij.codeInsight.TargetElementUtil
import com.intellij.codeInsight.navigation.ImplementationSearcher
import com.intellij.openapi.application.ReadAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.util.Ref
import com.intellij.psi.PsiElement
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.model.LanguageServerException
import com.ruin.lsp.model.positionToOffset
import com.ruin.lsp.util.ensureTargetElement
import com.ruin.lsp.util.getDocument
import com.ruin.lsp.util.location
import com.ruin.lsp.util.withEditor
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.Position

class FindImplementationCommand(val position: Position) : DocumentCommand<MutableList<Location>> {
    override fun execute(ctx: ExecutionContext): MutableList<Location> {
        val doc = getDocument(ctx.file)
            ?: throw LanguageServerException("No document found.")

        val offset = positionToOffset(doc, position)
        val ref: Ref<Array<PsiElement>?> = Ref()
        withEditor(this, ctx.file, position) { editor ->
            val element = ensureTargetElement(editor)
            ref.set(searchImplementations(editor, element, offset))
        }
        val implementations = ref.get()

        return implementations?.map{ it.location() }?.toMutableList() ?: mutableListOf()
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
