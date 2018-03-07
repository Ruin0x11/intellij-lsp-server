package com.ruin.lsp.commands.document.find

import com.intellij.openapi.editor.Document
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiMethod
import com.intellij.psi.search.searches.SuperMethodsSearch
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.model.LanguageServerException
import com.ruin.lsp.model.positionToOffset
import com.ruin.lsp.util.getDocument
import com.ruin.lsp.util.getURIForFile
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.Range

class FindDefinitionCommand(val position: Position) : DocumentCommand<MutableList<Location>> {
    override fun execute(ctx: ExecutionContext): MutableList<Location> {
        val doc = getDocument(ctx.file)
            ?: throw LanguageServerException("No document found.")

        val offset = positionToOffset(doc, position)
        val ref = ctx.file.findReferenceAt(offset)

        var lookup = ref?.resolve()

        if (lookup == null) {
            val element = ctx.file.findElementAt(offset)
            val parent = element?.parent
            if (parent != null && parent is PsiMethod) {
                val superSignature =
                    SuperMethodsSearch.search(parent, null, true, false).findFirst()
                lookup = superSignature?.method
            }
        }

        return if (lookup != null) {
            mutableListOf(elementToLocation(lookup))
        } else {
            mutableListOf()
        }
    }
}

fun offsetToPosition(doc: Document, offset: Int): Position {
    if (offset == -1) {
        return Position(0, 0)
    }
    val line = doc.getLineNumber(offset)
    val lineStartOffset = doc.getLineStartOffset(line)
    val column = offset - lineStartOffset
    return Position(line, column)
}

fun textRangeToRange(doc: Document, textRange: TextRange): Range =
    Range(
        offsetToPosition(doc, textRange.startOffset),
        offsetToPosition(doc, textRange.endOffset)
    )


fun elementToLocation(psi: PsiElement): Location {
    // TODO: support lookup of files inside JARs?
    val uri = getURIForFile(psi.containingFile)
    val doc = getDocument(psi.containingFile)!!
    val position = offsetToPosition(doc, psi.textOffset)
    return Location(uri, Range(position, position))
}

fun elementToLocationWithRange(psi: PsiElement): Location {
    val uri = getURIForFile(psi.containingFile)
    val doc = getDocument(psi.containingFile)!!
    val range = textRangeToRange(doc, psi.textRange)
    return Location(uri, range)
}
