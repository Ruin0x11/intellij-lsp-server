package com.ruin.lsp.util

import com.intellij.lang.jvm.JvmModifier
import com.intellij.openapi.editor.Document
import com.intellij.openapi.util.TextRange
import com.intellij.psi.*
import com.ruin.lsp.commands.document.hover.generateType
import com.ruin.lsp.values.DocumentUri
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.SymbolKind
import org.jetbrains.kotlin.KtNodeType
import org.jetbrains.kotlin.KtNodeTypes
import org.jetbrains.kotlin.asJava.namedUnwrappedElement
import org.jetbrains.kotlin.idea.intentions.loopToCallChain.isConstant
import org.jetbrains.kotlin.idea.refactoring.isCompanionMemberOf
import org.jetbrains.kotlin.idea.refactoring.memberInfo.qualifiedClassNameForRendering
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.psi.psiUtil.containingClass
import org.jetbrains.kotlin.psi.psiUtil.containingClassOrObject

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

fun PsiElement.location() =
    location(getURIForFile(this.containingFile), getDocument(this.containingFile)!!)

fun PsiElement.location(uri: DocumentUri, doc: Document) =
    ((this as? PsiNameIdentifierOwner)?.nameIdentifier ?: this).textRange.let { range ->
        Location(uri, Range(offsetToPosition(doc, range.startOffset), offsetToPosition(doc, range.endOffset)))
    }

fun PsiElement.symbolKind(): SymbolKind? =
    when (this) {
        is KtElement -> this.ktSymbolKind()

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

fun PsiElement.symbolName(): String? =
    when (this) {
        is KtElement -> ktSymbolName()

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
        is KtConstructor<*> -> SymbolKind.Constructor
        is KtFunction -> when {
            isInsideCompanion() -> SymbolKind.Function
            containingClass() != null -> SymbolKind.Method
            else -> SymbolKind.Function
        }
        is KtProperty -> when {
            isConstant(this) -> SymbolKind.Constant
            isMember -> SymbolKind.Field
            else -> SymbolKind.Variable
        }
        is KtVariableDeclaration -> SymbolKind.Variable
        is KtParameter -> SymbolKind.Variable
        is KtAnnotationEntry -> SymbolKind.Property
        is KtConstantExpression ->
            when (this.node.elementType) {
                KtNodeTypes.BOOLEAN_CONSTANT -> SymbolKind.Boolean
                KtNodeTypes.INTEGER_CONSTANT, KtNodeTypes.FLOAT_CONSTANT ->
                    SymbolKind.Number
                KtNodeTypes.STRING_TEMPLATE -> SymbolKind.String
                else -> SymbolKind.Constant
            }
        is KtStringTemplateExpression -> SymbolKind.String
        else -> null
    }


fun KtElement.ktSymbolName(): String? =
    when (this) {
        is KtFile -> name
        is KtPackageDirective -> qualifiedName
        is KtClass -> name ?: qualifiedClassNameForRendering()
        is KtImportDirective -> importedFqName?.asString() ?: "<error>"
        is KtClassInitializer -> name ?: "<init>"
        is KtFunction -> methodLabel(this)
        is KtProperty -> name
        is KtVariableDeclaration -> name
        is KtParameter -> name
        is KtAnnotationEntry -> annotationLabel(this)
        is KtConstantExpression -> text
        is KtStringTemplateExpression -> text

        else -> null
    }

private fun annotationLabel(annotation: PsiAnnotation): String =
    (annotation.nameReferenceElement?.text ?: annotation.qualifiedName)?.let { "@$it" }
        ?: "<unknown>"

private fun annotationLabel(annotation: KtAnnotationEntry): String =
    (annotation.typeReference?.text ?: annotation.name)?.let { "@$it" }
        ?: "<unknown>"

/** Return a method label including simplified parameter types. */
private fun methodLabel(method: PsiMethod): String =
    method.name + "(" + method.parameterList.parameters.joinToString(", ") { param ->
        methodParameterLabel(method, param)
    } + ")"

/** Return a method label including simplified parameter types. */
private fun methodLabel(method: KtFunction): String =
    method.name + "(" + method.valueParameters.joinToString(", ") { param ->
        param.typeReference?.text ?: "<unknown>"
    } + ")"

private fun methodParameterLabel(method: PsiMethod, parameter: PsiParameter): String =
    StringBuilder().apply { generateType(this, parameter.type, method, false, true) }.toString()

private fun isConstant(elt: KtProperty) = elt.modifierList?.getModifier(KtTokens.CONST_KEYWORD) != null

private fun KtDeclaration.isInsideCompanion() =
    (containingClassOrObject as? KtObjectDeclaration)?.isCompanion() == true
