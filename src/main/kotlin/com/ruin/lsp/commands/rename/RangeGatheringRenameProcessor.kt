package com.ruin.lsp.commands.rename

import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.DumbService
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Ref
import com.intellij.openapi.util.TextRange
import com.intellij.psi.*
import com.intellij.psi.impl.source.tree.CompositeElement
import com.intellij.psi.meta.PsiMetaOwner
import com.intellij.psi.meta.PsiWritableMetaData
import com.intellij.refactoring.rename.BindablePsiReference
import com.intellij.refactoring.rename.PsiElementRenameHandler
import com.intellij.refactoring.rename.RenameProcessor
import com.intellij.refactoring.rename.RenamePsiElementProcessor
import com.intellij.refactoring.rename.naming.AutomaticOverloadsRenamerFactory
import com.intellij.usageView.UsageInfo
import com.intellij.util.IncorrectOperationException
import com.ruin.lsp.commands.find.toRange
import com.ruin.lsp.util.getDocument

class RangeGatheringRenameProcessor(proj: Project, val element: PsiElement, val name: String)
    : RenameProcessor(proj, element, name, false, false) {
    val refs: MutableList<RenameRange> = mutableListOf()

    init {
        // Always rename overloaded methods.
        addRenamerFactory(AutomaticOverloadsRenamerAlwaysFactory())
    }

    override fun doRun() {
        val usagesRef: Ref<Array<UsageInfo>> = Ref()
        prepareRenaming(element, name, myAllRenames)
        usagesRef.set(DumbService.getInstance(myProject).runReadActionInSmartMode<Array<UsageInfo>> { findUsages() })
        if (!preprocessUsages(usagesRef)) return
        val newUsages = usagesRef.get()!!

        val classified = classifyUsages(myAllRenames.keys, newUsages)
        for (element in myAllRenames.keys) {
            val newName = myAllRenames[element] ?: continue

            val infos = classified.get(element)
            try {
                val toAdd = getRenames(element, newName, infos.toTypedArray())
                refs.addAll(toAdd)
            } catch (e: IncorrectOperationException) {
            }
        }
    }
}

/**
 * AutomaticRenamerFactory that will always rename overloaded methods.
 *
 * This can be made into a user config option at some point.
 */
internal class AutomaticOverloadsRenamerAlwaysFactory : AutomaticOverloadsRenamerFactory() {
    override fun isEnabled() = true
}

private fun canRenameElement(namedElement: PsiElement): Boolean {
    var writableMetaData: PsiWritableMetaData? = null
    if (namedElement is PsiMetaOwner) {
        val metaData = (namedElement as PsiMetaOwner).metaData
        if (metaData is PsiWritableMetaData) {
            writableMetaData = metaData
        }
    }
    return writableMetaData != null || namedElement is PsiNamedElement
}

private fun getRenames(namedElement: PsiElement, newName: String, usages: Array<UsageInfo>): List<RenameRange> {
    if (!canRenameElement(namedElement)) {
        LOG.error("Unknown element type:" + namedElement)
        return listOf()
    }

    // Rename the element itself.
    // When getting the text range, it has to be from the element's identifier, not the element itself (which
    // encompasses a larger area)
    val identifier = (namedElement as? PsiNameIdentifierOwner)?.nameIdentifier
    val elementRename = if (identifier != null) {
        RenameRange(identifier.textRange, identifier.containingFile, newName)
    } else {
        null
    }

    // Rename the places where the element is referenced.
    val usageRenames = getRenamesOfReferences(usages, newName)

    return if (elementRename != null) {
        usageRenames.plus(elementRename)
    } else {
        usageRenames
    }
}

private fun getRenamesOfReferences(usages: Array<UsageInfo>, newName: String): List<RenameRange> =
    usages.mapNotNull { usage ->
        val ref = usage.reference ?: return@mapNotNull null
        if (ref !is BindablePsiReference) {
            RenameRange(ref.element.localTextRange(), ref.element.containingFile, newName)
        } else
            null
    }

/**
 * Gets the TextRange from a PsiElement accounting for cases like assignment to a field with 'this'
 */
private fun PsiElement.localTextRange(): TextRange {
    // The reference may refer to an expression like "this.thing = thing"
    if (this is PsiReferenceExpression && this is CompositeElement) {
        if(this.firstChild is PsiThisExpression) {
            return this.lastChild.textRange
        }
    }
    return this.textRange
}

data class RenameRange(val textRange: TextRange, val file: PsiFile, val newName: String)
