package com.ruin.lsp.commands.document.symbol

import com.intellij.openapi.editor.Document
import com.intellij.psi.*
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.commands.document.hover.generateType
import com.ruin.lsp.model.LanguageServerException
import com.ruin.lsp.model.positionToOffset
import com.ruin.lsp.util.getDocument
import com.ruin.lsp.util.offsetToPosition
import com.ruin.lsp.util.symbolKind
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.SymbolInformation
import org.eclipse.lsp4j.TextDocumentIdentifier

class DocumentSymbolCommand(
    private val textDocumentIdentifier: TextDocumentIdentifier
) : DocumentCommand<MutableList<SymbolInformation>> {

    override fun execute(ctx: ExecutionContext): MutableList<SymbolInformation> {
        val uri = textDocumentIdentifier.uri
        val document = getDocument(ctx.file) ?: throw LanguageServerException("No document found.")
        val symbols = mutableListOf<SymbolInformation>()
        DocumentSymbolPsiVisitor(ctx.file, ctx.cancelToken) { element ->
            val kind = element.symbolKind()
            val name = element.symbolName()
            if (kind != null && name != null) {
                symbols.add(SymbolInformation(name, kind, element.toLocation(document, uri), element.containerName()))
            }
        }.visit()
        symbols.sortBy { positionToOffset(document, it.location.range.start) }
        return symbols.toMutableList()
    }
}

private fun PsiElement.symbolName(): String? =
    when (this) {
        is PsiFile -> name
        is PsiPackageStatement -> packageName
        is PsiImportStatement -> qualifiedName ?: "<error>"
        is PsiClass -> name ?: qualifiedName ?: "<anonymous>"
        is PsiClassInitializer -> name ?: "<init>"
        is PsiMethod -> methodLabel(this)
        is PsiEnumConstant -> name
        is PsiField -> name
        is PsiVariable -> name ?: "<unknown>"
        is PsiAnnotation -> annotationLabel(this)
        is PsiLiteralExpression -> text
        else -> null
    }


private fun PsiElement.containerName(): String? =
    generateSequence(parent, { it.parent })
        .firstOrNull { it.symbolKind() != null && it.symbolName() != null }
        ?.symbolName()

private fun PsiElement.toLocation(doc: Document, uri: String) =
    ((this as? PsiNameIdentifierOwner)?.nameIdentifier ?: this).textRange.let { range ->
        Location(uri, Range(offsetToPosition(doc, range.startOffset), offsetToPosition(doc, range.endOffset)))
    }

private fun annotationLabel(annotation: PsiAnnotation): String =
    (annotation.nameReferenceElement?.text ?: annotation.qualifiedName)?.let { "@$it" }
        ?: "<unknown>"

/** Return a method label including simplified parameter types. */
private fun methodLabel(method: PsiMethod): String =
    method.name + "(" + method.parameterList.parameters.joinToString(", ") { param ->
        methodParameterLabel(method, param)
    } + ")"

private fun methodParameterLabel(method: PsiMethod, parameter: PsiParameter): String =
    StringBuilder().apply { generateType(this, parameter.type, method, false, true) }.toString()
