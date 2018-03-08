package com.ruin.lsp.commands.document.hover

import com.intellij.codeInsight.documentation.DocumentationManager
import com.intellij.codeInsight.documentation.DocumentationManager.ORIGINAL_ELEMENT_KEY
import com.intellij.codeInsight.documentation.DocumentationManager.getProviderFromElement
import com.intellij.lang.documentation.DocumentationProvider
import com.intellij.lang.documentation.ExternalDocumentationProvider
import com.intellij.lang.java.JavaDocumentationProvider
import com.intellij.openapi.Disposable
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.application.ReadAction
import com.intellij.openapi.project.DumbService
import com.intellij.openapi.project.IndexNotReadyException
import com.intellij.openapi.util.NullableComputable
import com.intellij.openapi.util.Ref
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.PsiElement
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
        val ref: Ref<MutableList<Either<String, MarkedString>>> = Ref(mutableListOf())
        withEditor(this, ctx.file, position) { editor ->
            val originalElement = ctx.file.findElementAt(editor.caretModel.offset)

            val element = DocumentationManager.getInstance(ctx.project).findTargetElement(editor, ctx.file)

            if (element != null) {
                try {
                    val typeInfo = generateTypeInfo(element, originalElement)
                    val javadoc = generateJavadoc(element, originalElement)
                    val javadocUrls = generateJavadocUrls(element, originalElement)
                    if (typeInfo != null) {
                        ref.get().add(Either.forRight<String, MarkedString>(typeInfo))
                    }
                    if (javadoc != null) {
                        ref.get().add(Either.forLeft<String, MarkedString>(javadoc))
                    }
                    if (javadocUrls != null) {
                        ref.get().add(Either.forLeft<String, MarkedString>(javadocUrls))
                    }
                } catch (ex: IndexNotReadyException) {
                }
            }
        }

        return Hover(ref.get())
    }
}

fun generateTypeInfo(element: PsiElement, originalElement: PsiElement?): MarkedString? =
    MarkedString("java", HoverDocumentationProvider().generateDoc(element, originalElement))

fun generateJavadoc(element: PsiElement, originalElement: PsiElement?): String? {
    var javadoc = JavaDocumentationProvider().generateDoc(element, originalElement) ?: return null
    javadoc = StringUtil.stripHtml(javadoc, false)
    javadoc = StringUtil.unescapeXml(javadoc)
    return javadoc
}

fun generateJavadocUrls(element: PsiElement, myOriginalElement: PsiElement?): String? {
    val provider = ReadAction.compute<DocumentationProvider, RuntimeException> {
        getProviderFromElement(element, myOriginalElement)
    }

    return if (provider is ExternalDocumentationProvider) {
        val urls: List<String>? = ApplicationManager.getApplication().runReadAction( NullableComputable<List<String>>{
            val originalElement = element.getUserData(ORIGINAL_ELEMENT_KEY)?.element
            if ((provider as ExternalDocumentationProvider).hasDocumentationFor(element, originalElement)) {
                provider.getUrlFor(element, originalElement)
            } else {
                null
            }
        })

        urls?.joinToString("\n")
    } else {
        null
    }
}
