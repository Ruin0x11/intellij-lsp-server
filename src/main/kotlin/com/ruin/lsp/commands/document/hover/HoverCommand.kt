package com.ruin.lsp.commands.document.hover

import com.intellij.codeInsight.documentation.DocumentationManager
import com.intellij.lang.Language
import com.intellij.lang.documentation.AbstractDocumentationProvider
import com.intellij.lang.documentation.DocumentationProvider
import com.intellij.lang.java.JavaLanguage
import com.intellij.openapi.Disposable
import com.intellij.openapi.project.DumbService
import com.intellij.openapi.project.IndexNotReadyException
import com.intellij.openapi.util.Ref
import com.intellij.psi.PsiFile
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.util.withEditor
import org.eclipse.lsp4j.Hover
import org.eclipse.lsp4j.MarkedString
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.jetbrains.kotlin.idea.KotlinLanguage

class HoverCommand(val position: Position) : DocumentCommand<Hover>, Disposable {
    override fun execute(ctx: ExecutionContext): Hover {
        if (DumbService.isDumb(ctx.project)) {
            return Hover(mutableListOf())
        }
        val ref: Ref<MarkedString> = Ref(MarkedString("", ""))
        withEditor(this, ctx.file, position) { editor ->
            val originalElement = ctx.file.findElementAt(editor.caretModel.offset)

            val element = DocumentationManager.getInstance(ctx.project).findTargetElement(editor, ctx.file)

            if (element != null) {
                try {
                    val lang = element.language
                    val result = provider(lang)?.generateDoc(element, originalElement)?.trim() ?: ""
                    ref.set(MarkedString(lang.displayName.toLowerCase(), result))
                } catch (ex: IndexNotReadyException) {
                }
            }
        }

        val markedString = ref.get()

        return if (markedString.value.isEmpty())
            Hover(mutableListOf())
        else
            Hover(mutableListOf(Either.forRight<String, MarkedString>(markedString)))
    }

    private fun provider(language: Language): DocumentationProvider? {
        return when(language) {
            is JavaLanguage -> HoverDocumentationProvider()
            is KotlinLanguage -> HoverDocumentationProviderKt()
            else -> null
        }
    }
}
