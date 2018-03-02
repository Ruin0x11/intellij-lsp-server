package com.ruin.intel.commands.completion

import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.*
import com.ruin.intel.values.CompletionItem
import com.intellij.psi.PsiVariable
import com.intellij.psi.PsiPackage
import com.ruin.intel.values.InsertTextFormat


abstract class CompletionDecorator<T: PsiElement>(val lookup: LookupElement, val elt: T) {
    val completionItem: CompletionItem
    get() = CompletionItem(
        label = formatLabel(),
        insertText = formatInsertText(),
        documentation = formatDoc(),
        insertTextFormat = insertTextFormat)

    val clientSupportsSnippets = false
    val insertTextFormat: Int
        get() = if (clientSupportsSnippets) InsertTextFormat.SNIPPET else InsertTextFormat.PLAIN_TEXT

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
        fun from(lookup: LookupElement): CompletionDecorator<out PsiElement>? {
            val psi = lookup.psiElement
            return when (psi) {
                is PsiMethod -> MethodCompletionDecorator(lookup, psi)
                is PsiClass -> ClassCompletionDecorator(lookup, psi)
                is PsiField -> FieldCompletionDecorator(lookup, psi)
                is PsiVariable -> VariableCompletionDecorator(lookup, psi)
                is PsiPackage -> PackageCompletionDecorator(lookup, psi)
                else -> null
            }
        }
    }
}

class MethodCompletionDecorator(lookup: LookupElement, val method: PsiMethod)
    : CompletionDecorator<PsiMethod>(lookup, method) {
    override fun formatInsertText(): String {
        val parameterList = method.getParameterList()
        val params = if (clientSupportsSnippets)
            buildSnippetTabStops(method)
        else
            buildParens(method)
        return super.formatInsertText() + params
    }

    override fun formatLabel() =
        "${super.formatInsertText()}${buildParamsList(method)} -> ${getTypeName(method.returnType)}"
}

class ClassCompletionDecorator(lookup: LookupElement, val klass: PsiClass)
    : CompletionDecorator<PsiClass>(lookup, klass) {
    override fun formatLabel() =
        klass.qualifiedName ?: klass.toString()
}

class FieldCompletionDecorator(lookup: LookupElement, val field: PsiField)
    : CompletionDecorator<PsiField>(lookup, field) {
    override fun formatLabel() =
        "${field.type.presentableText}  ${field.name}"
}
class VariableCompletionDecorator(lookup: LookupElement, val variable: PsiVariable)
    : CompletionDecorator<PsiVariable>(lookup, variable) {
    private val type: String
    get() = variable.type.presentableText

    override fun formatLabel() = type

    override fun formatDoc(): String = "$type ${variable.name};"
}

class PackageCompletionDecorator(lookup: LookupElement, val pack: PsiPackage)
    : CompletionDecorator<PsiPackage>(lookup, pack) {

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

fun buildParens(method: PsiMethod) = if (method.parameters.isEmpty()) "()" else "("

fun buildSnippetTabStops(method: PsiMethod): CharSequence {
    val tabStops = method.parameterList.parameters.map {
        "${'$'}${it.name}"
    }.joinToString(", ")

    return "($tabStops)$0"
}

fun getTypeName(type: PsiType?): CharSequence {
    return type?.presentableText ?: "void"

}

fun buildModifierList(method: PsiMethod): CharSequence {
    return method.modifierList.text
}
