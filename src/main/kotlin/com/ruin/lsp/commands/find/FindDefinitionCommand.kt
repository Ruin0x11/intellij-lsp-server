package com.ruin.lsp.commands.find

import com.github.kittinunf.result.Result
import com.intellij.openapi.editor.Document
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.PsiMethod
import com.intellij.psi.search.searches.SuperMethodsSearch
import com.ruin.lsp.util.*
import com.ruin.lsp.commands.Command
import com.ruin.lsp.commands.errorResult
import com.ruin.lsp.commands.highlight.textRangeToRange
import com.ruin.lsp.model.positionToOffset
import com.ruin.lsp.values.Location
import com.ruin.lsp.values.Position
import com.ruin.lsp.values.Range
import com.ruin.lsp.values.TextDocumentIdentifier

class FindDefinitionCommand(val textDocumentIdentifier: TextDocumentIdentifier,
                            val position: Position) : Command<List<Location>> {
    override fun execute(project: Project, file: PsiFile): Result<List<Location>, Exception> {
        val doc = getDocument(file)
            ?: return errorResult("No document found.")

        val offset = positionToOffset(doc, position)
        val ref = file.findReferenceAt(offset)

        var lookup = ref?.resolve()

        if (lookup == null) {
            val element = file.findElementAt(offset)
            val parent = element?.parent
            if (parent != null && parent is PsiMethod) {
                val superSignature =
                    SuperMethodsSearch.search(parent, null, true, false).findFirst()
                lookup = superSignature?.method
            }
        }

        return if (lookup != null) {
            Result.of(listOf(elementToLocation(lookup)))
        } else {
            Result.of(listOf())
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

fun TextRange.toRange(doc: Document): Range =
    Range(
        offsetToPosition(doc, this.startOffset),
        offsetToPosition(doc, this.endOffset)
    )


fun elementToLocation(psi: PsiElement): Location {
    // TODO: support lookup of files inside JARs?
    val uri = getURIForFile(psi.containingFile)
    val doc = getDocument(psi.containingFile)!!
    val position = offsetToPosition(doc, psi.textOffset)
    return Location(uri, Range(position, position))
}

fun PsiElement.toRange(doc: Document) = this.textRange.toRange(doc)

fun elementToLocationWithRange(psi: PsiElement): Location {
    val uri = getURIForFile(psi.containingFile)
    val doc = getDocument(psi.containingFile)!!
    val range = psi.toRange(doc)
    return Location(uri, range)
}
