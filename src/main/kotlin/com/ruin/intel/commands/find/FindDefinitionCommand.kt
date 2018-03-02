package com.ruin.intel.commands.find

import com.github.kittinunf.result.Result
import com.intellij.ide.util.PsiNavigationSupport
import com.intellij.openapi.editor.Document
import com.intellij.openapi.fileEditor.OpenFileDescriptor
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.PsiMethod
import com.intellij.psi.search.searches.SuperMethodsSearch
import com.ruin.intel.util.*
import com.ruin.intel.commands.Command
import com.ruin.intel.commands.errorResult
import com.ruin.intel.model.positionToOffset
import com.ruin.intel.values.Location
import com.ruin.intel.values.Position
import com.ruin.intel.values.Range
import com.ruin.intel.values.TextDocumentIdentifier

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
            Result.of(listOf(toLocation(lookup)))
        } else {
            return errorResult("No definition found.")
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

fun toLocation(psi: PsiElement): Location {
    // TODO: support lookup of files inside JARs?
    val uri = getURIForFile(psi.containingFile)
    val doc = getDocument(psi.containingFile)!!
    val position = offsetToPosition(doc, psi.textOffset)
    return Location(uri, Range(position, position))
}

