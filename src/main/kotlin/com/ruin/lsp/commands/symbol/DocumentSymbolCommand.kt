package com.ruin.lsp.commands.symbol

import com.github.kittinunf.result.Result
import com.intellij.lang.jvm.JvmModifier
import com.intellij.openapi.editor.Document
import com.intellij.openapi.project.Project
import com.intellij.psi.*
import com.ruin.lsp.commands.Command
import com.ruin.lsp.commands.errorResult
import com.ruin.lsp.commands.find.offsetToPosition
import com.ruin.lsp.commands.hover.generateType
import com.ruin.lsp.model.positionToOffset
import com.ruin.lsp.util.getDocument
import com.ruin.lsp.values.*

class DocumentSymbolCommand(
    private val textDocumentIdentifier: TextDocumentIdentifier
) : Command<List<SymbolInformation>> {

    override fun execute(project: Project, file: PsiFile): Result<List<SymbolInformation>, Exception> {
        val uri = textDocumentIdentifier.uri
        val document = getDocument(file) ?: return errorResult("No document found.")
        val symbols = mutableListOf<SymbolInformation>()
        DocumentSymbolPsiVisitor(file) { element ->
            val kind = element.symbolKind()
            val name = element.symbolName()
            if (kind != null && name != null) {
                symbols.add(SymbolInformation(name, kind, element.toLocation(document, uri), element.containerName()))
            }
        }.visit()
        symbols.sortBy { positionToOffset(document, it.location.range.start) }
        return Result.of(symbols)
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

private fun PsiElement.symbolKind(): Int? =
    when (this) {
        is PsiFile -> SymbolKind.FILE
        is PsiPackageStatement -> SymbolKind.PACKAGE
        is PsiImportStatement -> SymbolKind.MODULE
        is PsiClass -> when {
            isAnnotationType || isInterface -> SymbolKind.INTERFACE
            isEnum -> SymbolKind.ENUM
            else -> SymbolKind.CLASS
        }
        is PsiClassInitializer -> SymbolKind.CONSTRUCTOR
        is PsiMethod -> if (isConstructor) SymbolKind.CONSTRUCTOR else SymbolKind.METHOD
        is PsiEnumConstant -> SymbolKind.ENUM_MEMBER
        is PsiField ->
            if (hasModifier(JvmModifier.STATIC) && hasModifier(JvmModifier.FINAL)) {
                SymbolKind.CONSTANT
            } else {
                SymbolKind.FIELD
            }
        is PsiVariable -> SymbolKind.VARIABLE
        is PsiAnnotation -> SymbolKind.PROPERTY
        is PsiLiteralExpression -> {
            (type as? PsiClassType)?.let { if (it.name == "String") SymbolKind.STRING else null }
                ?: when (this.type) {
                    PsiType.BOOLEAN -> SymbolKind.BOOLEAN
                    PsiType.BYTE, PsiType.DOUBLE, PsiType.FLOAT, PsiType.INT, PsiType.LONG, PsiType.SHORT ->
                        SymbolKind.NUMBER
                    PsiType.CHAR -> SymbolKind.STRING
                    PsiType.NULL, PsiType.VOID -> SymbolKind.NULL
                    else -> SymbolKind.CONSTANT
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
    (annotation.nameReferenceElement?.text ?: annotation.qualifiedName)?.let { "@$it"}
        ?: "<unknown>"

/** Return a method label including simplified parameter types. */
private fun methodLabel(method: PsiMethod): String =
    method.name + "(" + method.parameterList.parameters.joinToString(", ") { param ->
        methodParameterLabel(method, param)
    } + ")"

private fun methodParameterLabel(method: PsiMethod, parameter: PsiParameter): String =
    StringBuilder().apply { generateType(this, parameter.type, method, false, true) }.toString()
