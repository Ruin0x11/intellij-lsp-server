package com.ruin.intel.commands.find

import com.github.kittinunf.result.Result
import com.intellij.openapi.editor.Document
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.ruin.intel.Util.*
import com.ruin.intel.commands.Command
import com.ruin.intel.model.LanguageServerException
import com.ruin.intel.model.positionToOffset
import com.ruin.intel.values.Location
import com.ruin.intel.values.Position
import com.ruin.intel.values.Range
import com.ruin.intel.values.TextDocumentIdentifier

class FindDefinitionCommand(val textDocumentIdentifier: TextDocumentIdentifier,
                            val position: Position) : Command<List<Location>> {
    override fun execute(project: Project, file: PsiFile): Result<List<Location>, Exception> {
        val doc = getDocument(file)
            ?: return Result.error(LanguageServerException("No reference found."))

        val offset = positionToOffset(doc, position)
        val ref = file.findReferenceAt(offset)
            ?: return Result.error(LanguageServerException("No reference found."));

        val lookup = ref.resolve()
            ?: return Result.error(LanguageServerException("Definition not found."));

        return Result.of(listOf(toLocation(lookup)))
    }
}

fun offsetToPosition(doc: Document, offset: Int): Position {
    val line = doc.getLineNumber(offset)
    val lineStartOffset = doc.getLineStartOffset(line)
    val column = offset - lineStartOffset
    return Position(line, column)
}

fun toLocation(psi: PsiElement): Location {
    // TODO: support lookup of files inside JARs?
    val uri = getURIForFile(psi.containingFile)
    val doc = getDocument(psi.containingFile)!!
    val position = offsetToPosition(doc, psi.textOffset)
    return Location(uri, Range(position, position))
}

