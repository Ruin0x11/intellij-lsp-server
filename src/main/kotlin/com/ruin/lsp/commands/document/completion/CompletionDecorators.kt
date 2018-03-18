package com.ruin.lsp.commands.document.completion

import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.*
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.CompletionItemKind
import org.eclipse.lsp4j.InsertTextFormat


abstract class CompletionDecorator<out T : PsiElement>(val lookup: LookupElement, val elt: T) {
    val completionItem: CompletionItem
        get() = CompletionItem().apply {
            label = formatLabel()
            kind = kind
            insertText = formatInsertText()
            documentation = formatDoc()
            insertTextFormat = myInsertTextFormat
        }

    var clientSupportsSnippets = false
    private val myInsertTextFormat: InsertTextFormat
        get() = if (clientSupportsSnippets) InsertTextFormat.Snippet else InsertTextFormat.PlainText
    abstract val kind: CompletionItemKind

    protected open fun formatInsertText(): String {
        if (elt is PsiNamedElement) {
            val name = elt.name
            if (!StringUtil.isEmpty(name)) {
                // use it. This fixes cases like Arrays.asList() where
                //  the lookup string contains the class (but we've
                //  already typed it)
                return name!!
            }
        }

        // fallback
        return lookup.lookupString
    }

    protected open fun formatDoc(): String {
        return buildDocComment(elt as PsiDocCommentOwner)
    }

    protected abstract fun formatLabel(): String

    companion object {
        fun from(lookup: LookupElement, snippetSupport: Boolean): CompletionDecorator<PsiElement>? {
            val psi = lookup.psiElement
            val decorator = when (psi) {
                is PsiMethod -> MethodCompletionDecorator(lookup, psi)
                is PsiClass -> ClassCompletionDecorator(lookup, psi)
                is PsiField -> FieldCompletionDecorator(lookup, psi)
                is PsiVariable -> VariableCompletionDecorator(lookup, psi)
                is PsiPackage -> PackageCompletionDecorator(lookup, psi)
                else -> null
            }
            decorator?.clientSupportsSnippets = snippetSupport
            return decorator
        }
    }
}

class MethodCompletionDecorator(lookup: LookupElement, val method: PsiMethod)
    : CompletionDecorator<PsiMethod>(lookup, method) {
    override val kind = CompletionItemKind.Method

    override fun formatInsertText() =
        super.formatInsertText() + buildMethodParams(method, clientSupportsSnippets)

    override fun formatLabel() =
        "${super.formatInsertText()}${buildParamsList(method)} : ${getTypeName(method.returnType)}"
}

class ClassCompletionDecorator(lookup: LookupElement, val klass: PsiClass)
    : CompletionDecorator<PsiClass>(lookup, klass) {
    override val kind = CompletionItemKind.Class

    override fun formatLabel() =
        klass.qualifiedName ?: klass.toString()
}

class FieldCompletionDecorator(lookup: LookupElement, val field: PsiField)
    : CompletionDecorator<PsiField>(lookup, field) {
    override val kind = CompletionItemKind.Field

    override fun formatLabel() =
        "${field.name} : ${field.type.presentableText}"
}

class VariableCompletionDecorator(lookup: LookupElement, val variable: PsiVariable)
    : CompletionDecorator<PsiVariable>(lookup, variable) {
    override val kind = CompletionItemKind.Variable

    private val type = variable.type.presentableText

    override fun formatLabel() = "${variable.name} : $type"

    override fun formatDoc(): String = "$type ${variable.name};"
}

class PackageCompletionDecorator(lookup: LookupElement, val pack: PsiPackage)
    : CompletionDecorator<PsiPackage>(lookup, pack) {
    override val kind = CompletionItemKind.Module

    override fun formatLabel() = pack.qualifiedName

    override fun formatDoc() = pack.qualifiedName
}

fun buildDocComment(method: PsiDocCommentOwner): String {
    val docComment = method.docComment ?: return ""

    return docComment.text
}

fun buildParamsList(method: PsiMethod): CharSequence {
    val builder = StringBuilder(128)
    builder.append('(')

    var first = true
    val parameterList = method.parameterList
    for (param in parameterList.parameters) {
        if (first) {
            first = false
        } else {
            builder.append(", ")
        }

        builder.append(getTypeName(param.type))
            .append(' ')
            .append(param.name)
    }

    builder.append(')')
    return builder.toString()
}

fun buildMethodParams(method: PsiMethod, snippetSupport: Boolean) = if (snippetSupport)
    buildSnippetTabStops(method)
else
    buildParens(method)

fun buildParens(method: PsiMethod) = if (method.parameters.isEmpty()) "()" else "("

fun buildSnippetTabStops(method: PsiMethod): CharSequence {
    val tabStops = method.parameterList.parameters.mapIndexed(::methodParamToSnippet).joinToString(", ")

    return "($tabStops)$0"
}

fun methodParamToSnippet(index: Int, param: PsiParameter) =
    "${'$'}${'{'}${index+1}:${param.name}${'}'}"

fun getTypeName(type: PsiType?) =
    type?.presentableText ?: "void"

