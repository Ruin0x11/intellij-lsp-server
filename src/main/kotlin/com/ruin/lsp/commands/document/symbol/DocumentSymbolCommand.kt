package com.ruin.lsp.commands.document.symbol

import com.intellij.lang.jvm.JvmModifier
import com.intellij.openapi.editor.Document
import com.intellij.psi.*
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.commands.document.find.offsetToPosition
import com.ruin.lsp.commands.document.hover.generateType
import com.ruin.lsp.model.LanguageServerException
import com.ruin.lsp.model.positionToOffset
import com.ruin.lsp.util.getDocument
import org.eclipse.lsp4j.*

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

private fun PsiElement.symbolKind(): SymbolKind? =
    when (this) {
        is PsiFile -> SymbolKind.File
        is PsiPackageStatement -> SymbolKind.Package
        is PsiImportStatement -> SymbolKind.Module
        is PsiClass -> when {
            isAnnotationType || isInterface -> SymbolKind.Interface
            isEnum -> SymbolKind.Enum
            else -> SymbolKind.Class
        }
        is PsiClassInitializer -> SymbolKind.Constructor
        is PsiMethod -> if (isConstructor) SymbolKind.Constructor else SymbolKind.Method
        is PsiEnumConstant -> SymbolKind.Enum // TODO: Replace when lsp4j has EnumMember
        is PsiField ->
            if (hasModifier(JvmModifier.STATIC) && hasModifier(JvmModifier.FINAL)) {
                SymbolKind.Constant
            } else {
                SymbolKind.Field
            }
        is PsiVariable -> SymbolKind.Variable
        is PsiAnnotation -> SymbolKind.Property
        is PsiLiteralExpression -> {
            (type as? PsiClassType)?.let { if (it.name == "String") SymbolKind.String else null }
                ?: when (this.type) {
                    PsiType.BOOLEAN -> SymbolKind.Boolean
                    PsiType.BYTE, PsiType.DOUBLE, PsiType.FLOAT, PsiType.INT, PsiType.LONG, PsiType.SHORT ->
                        SymbolKind.Number
                    PsiType.CHAR -> SymbolKind.String
                // PsiType.NULL, PsiType.VOID -> SymbolKind.Null // TODO: Add when lsp4j has Null
                    else -> SymbolKind.Constant
                }
        }
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
