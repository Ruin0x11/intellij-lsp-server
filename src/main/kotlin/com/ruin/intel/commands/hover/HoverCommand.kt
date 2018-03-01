package com.ruin.intel.commands.hover

import com.github.kittinunf.result.Result
import com.intellij.codeInsight.documentation.DocumentationManager
import com.intellij.openapi.Disposable
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.EditorFactory
import com.ruin.intel.Util.createEditor
import com.ruin.intel.Util.resolvePsiFromUri
import com.ruin.intel.commands.completion.Command
import com.ruin.intel.values.MarkedString
import com.ruin.intel.values.Position
import com.ruin.intel.values.TextDocumentIdentifier

class HoverCommand(val textDocumentIdentifier: TextDocumentIdentifier,
                        val position: Position) : Command<MarkedString>, Disposable {
    private val LOG = Logger.getInstance(HoverCommand::class.java)

    // TODO: Use invokeAndWait + Executor to get result makeCompletionParameters Future instead
    override fun execute(): Result<MarkedString, Exception> {
        var result: String? = null
        ApplicationManager.getApplication().invokeAndWait {
            val pair = resolvePsiFromUri(textDocumentIdentifier.uri)
            if (pair != null) {
                val (project, file) = pair

                val editor = createEditor(this, file, position.line, position.character)
                val originalElement = file.findElementAt(editor.caretModel.offset)
                val element = DocumentationManager.getInstance(project).findTargetElement(editor, file)
                result = if (element != null) {
                    // TODO: might want to use something like CtrlMouseHandler instead
                    HoverDocumentationProvider().generateDoc(element, originalElement) ?: ""
                } else {
                    ""
                }
                // Disposer doesn't release editor after registering in createEditor?
                val editorFactory = EditorFactory.getInstance()
                editorFactory.releaseEditor(editor)
            } else {
                LOG.warn("Couldn't resolve PSI makeCompletionParameters ${textDocumentIdentifier.uri}.")
            }
        }

        return Result.of(result)
    }
}
