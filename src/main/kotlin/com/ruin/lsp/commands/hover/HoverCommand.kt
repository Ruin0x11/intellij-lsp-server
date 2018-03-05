package com.ruin.lsp.commands.hover

import com.intellij.codeInsight.documentation.DocumentationManager
import com.intellij.openapi.Disposable
import com.intellij.openapi.project.IndexNotReadyException
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Ref
import com.intellij.psi.PsiFile
import com.ruin.lsp.commands.Command
import com.ruin.lsp.util.withEditor
import org.eclipse.lsp4j.Hover
import org.eclipse.lsp4j.MarkedString
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.jsonrpc.messages.Either

class HoverCommand(val position: Position) : Command<Hover>, Disposable {
    override fun execute(project: Project, file: PsiFile): Hover {
        val ref: Ref<String> = Ref()
        withEditor(this, file, position) { editor ->
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

        val markedString = MarkedString("java", ref.get())

        return if (markedString.value.isEmpty())
            Hover(mutableListOf())
        else
            Hover(mutableListOf(Either.forRight<String, MarkedString>(markedString)))
    }
}
