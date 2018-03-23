package com.ruin.lsp.util

import com.intellij.lang.jvm.JvmModifier
import com.intellij.openapi.editor.Document
import com.intellij.openapi.util.TextRange
import com.intellij.psi.*
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.SymbolKind
import org.jetbrains.kotlin.asJava.elements.KtLightField
import org.jetbrains.kotlin.asJava.elements.KtLightMethod
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.psi.stubs.elements.KtClassElementType
import org.jetbrains.kotlin.psi.stubs.elements.KtStubElementType

fun Position.toOffset(doc: Document) = doc.getLineStartOffset(this.line) + this.character

fun Range.toTextRange(doc: Document) =
    TextRange(
        this.start.toOffset(doc),
        this.end.toOffset(doc)
    )

fun offsetToPosition(doc: Document, offset: Int): Position {
    if (offset == -1) {
        return Position(0, 0)
    }
    val line = doc.getLineNumber(offset)
    val lineStartOffset = doc.getLineStartOffset(line)
    val column = offset - lineStartOffset
    return Position(line, column)
}

fun TextRange.toRange(doc: Document): Range =
    Range(
        offsetToPosition(doc, this.startOffset),
        offsetToPosition(doc, this.endOffset)
    )


fun PsiElement.location(): Location {
    // TODO: support lookup of files inside JARs?
    val uri = getURIForFile(this.containingFile)
    val doc = getDocument(this.containingFile)!!
    val position = offsetToPosition(doc, this.textOffset)
    return Location(uri, Range(position, position))
}

fun PsiElement.symbolKind(): SymbolKind? =
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
        is KtElement -> this.ktSymbolKind()
        else -> null
    }

fun KtElement.ktSymbolKind(): SymbolKind? =
    when (this) {
        is KtFile -> SymbolKind.File
        is KtPackageDirective -> SymbolKind.Package
        is KtImportDirective -> SymbolKind.Module
        is KtClass -> when {
            isInterface() -> SymbolKind.Interface
            isEnum() -> SymbolKind.Enum
            else -> SymbolKind.Class
        }
        is KtClassInitializer -> SymbolKind.Constructor
        is KtLightMethod -> if (isConstructor) SymbolKind.Constructor else SymbolKind.Method
        is KtFunction -> SymbolKind.Function
        is KtLightField ->
            if (hasModifier(JvmModifier.STATIC) && hasModifier(JvmModifier.FINAL)) {
                SymbolKind.Constant
            } else {
                SymbolKind.Field
            }
        is KtVariableDeclaration -> SymbolKind.Variable
        is KtAnnotation -> SymbolKind.Property
        else -> null
    }
