package com.ruin.lsp.commands.document.hover

import com.intellij.codeInsight.documentation.DocumentationManager
import com.intellij.openapi.Disposable
import com.intellij.openapi.project.DumbService
import com.intellij.openapi.project.IndexNotReadyException
import com.intellij.openapi.project.ProjectManager
import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.openapi.util.Ref
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.util.withEditor
import org.eclipse.lsp4j.Hover
import org.eclipse.lsp4j.MarkedString
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.jsonrpc.messages.Either

class HoverCommand(val position: Position) : DocumentCommand<Hover>, Disposable {
    override fun execute(ctx: ExecutionContext): Hover {
        if (DumbService.isDumb(ctx.project)) {
            return Hover(mutableListOf())
        }
        val ref: Ref<String> = Ref()
        withEditor(this, ctx.file, position) { editor ->
            val originalElement = ctx.file.findElementAt(editor.caretModel.offset)

            val element = DocumentationManager.getInstance(ctx.project).findTargetElement(editor, ctx.file)

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
