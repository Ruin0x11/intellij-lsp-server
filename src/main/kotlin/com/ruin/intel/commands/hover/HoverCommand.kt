package com.ruin.intel.commands.hover

import com.github.kittinunf.result.Result
import com.intellij.codeInsight.documentation.DocumentationManager
import com.intellij.idea.ActionsBundle
import com.intellij.openapi.Disposable
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.project.DumbService
import com.intellij.openapi.project.IndexNotReadyException
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Ref
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.ruin.intel.Util.createEditor
import com.ruin.intel.Util.resolvePsiFromUri
import com.ruin.intel.Util.withEditor
import com.ruin.intel.commands.Command
import com.ruin.intel.commands.errorResult
import com.ruin.intel.values.MarkedString
import com.ruin.intel.values.Position
import com.ruin.intel.values.TextDocumentIdentifier

class HoverCommand(val textDocumentIdentifier: TextDocumentIdentifier,
                        val position: Position) : Command<MarkedString>, Disposable {
    override fun execute(project: Project, file: PsiFile): Result<MarkedString, Exception> {

        val ref: Ref<String> = Ref()
        withEditor(this, file, position) {editor ->
            val originalElement = file.findElementAt(editor.caretModel.offset)

            val element = DocumentationManager.getInstance(project).findTargetElement(editor, file)

            val result = if (element != null) {
                // TODO: might want to use something like CtrlMouseHandler instead
                try {
                    HoverDocumentationProvider().generateDoc(element, originalElement) ?: ""
                } catch (ex: IndexNotReadyException) {
                    ""
                }
            } else {
                ""
            }
            ref.set(result)
        }

        val language = "java"

        return Result.of(MarkedString(language, ref.get()))
    }
}
