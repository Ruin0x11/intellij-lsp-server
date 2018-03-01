package com.ruin.intel.commands.definition

import com.github.kittinunf.result.Result
import com.intellij.codeInsight.TargetElementUtil
import com.intellij.codeInsight.navigation.actions.GotoDeclarationAction
import com.intellij.debugger.SourcePosition
import com.intellij.extapi.psi.PsiElementBase
import com.intellij.ide.util.EditSourceUtil
import com.intellij.ide.util.PsiNavigationSupport
import com.intellij.lang.LanguageNamesValidation
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.fileEditor.OpenFileDescriptor
import com.intellij.openapi.project.DumbService
import com.intellij.openapi.project.IndexNotReadyException
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Computable
import com.intellij.pom.Navigatable
import com.intellij.psi.PsiClass
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.PsiReference
import com.intellij.psi.util.PsiUtilCore
import com.ruin.intel.Util.*
import com.ruin.intel.commands.completion.Command
import com.ruin.intel.model.LanguageServerException
import com.ruin.intel.model.positionToOffset
import com.ruin.intel.values.Location
import com.ruin.intel.values.Position
import com.ruin.intel.values.Range
import com.ruin.intel.values.TextDocumentIdentifier
import java.lang.Integer.max
import java.util.*

class DefinitionCommand(val textDocumentIdentifier: TextDocumentIdentifier,
                        val position: Position) : Command<Location> {
    override fun execute(): Result<Location, Exception> {
        return invokeAndWaitIfNeeded( Computable<Result<Location, Exception>> {
            val pair = resolvePsiFromUri(textDocumentIdentifier.uri)
            if (pair != null) {
                val (_, file) = pair

                val doc = getDocument(file)

                if (doc != null) {
                    val offset = positionToOffset(doc, position)
                    val ref = file.findReferenceAt(offset)
                        ?: return@Computable Result.error(LanguageServerException("No reference found."));

                    val lookup = ref.resolve()
                        ?: return@Computable Result.error(LanguageServerException("Definition not found."));

                    return@Computable Result.of(toLocation(lookup))
                }
            }
            Result.error(LanguageServerException("No reference found."))
        })
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

