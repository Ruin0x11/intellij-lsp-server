package com.ruin.lsp.commands.document.completion

import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.*
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.CompletionItemKind
import org.eclipse.lsp4j.InsertTextFormat
import org.eclipse.lsp4j.SymbolKind
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.descriptors.FunctionDescriptor
import org.jetbrains.kotlin.descriptors.PropertyDescriptor
import org.jetbrains.kotlin.descriptors.ValueParameterDescriptor
import org.jetbrains.kotlin.descriptors.impl.LocalVariableDescriptor
import org.jetbrains.kotlin.idea.completion.DeclarationLookupObjectImpl
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.resolve.calls.callUtil.getType
import org.jetbrains.kotlin.resolve.descriptorUtil.classValueType
import org.jetbrains.kotlin.resolve.descriptorUtil.fqNameSafe
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.types.SimpleType


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
        return buildDocComment(elt)
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

                is KtElement -> fromKotlin(lookup)
                else -> null
            }
            decorator?.clientSupportsSnippets = snippetSupport
            return decorator
        }

        fun fromKotlin(lookup: LookupElement): CompletionDecorator<PsiElement>? {
            val descriptor = (lookup.`object` as? DeclarationLookupObjectImpl)?.descriptor ?: return null
            val psi = lookup.psiElement
            return when (descriptor) {
                is org.jetbrains.kotlin.descriptors.VariableDescriptor -> {
                    val kType = descriptor.type
                    KtVariableCompletionDecorator(lookup, psi as KtProperty, kType)
                }
                is org.jetbrains.kotlin.descriptors.PropertyDescriptor -> {
                    val kType = descriptor.returnType ?: descriptor.type
                    KtPropertyCompletionDecorator(lookup, psi as KtProperty, kType)
                }
                is org.jetbrains.kotlin.descriptors.FunctionDescriptor -> {
                    if (psi is KtNamedFunction) {
                        val kType = descriptor.returnType
                        val args = descriptor.valueParameters
                        KtFunctionCompletionDecorator(lookup, psi, kType, args)
                    } else {
                        null
                    }
                }
                is org.jetbrains.kotlin.descriptors.ClassDescriptor -> {
                    val kType = descriptor.classValueType ?: descriptor.defaultType
                    KtClassCompletionDecorator(lookup, psi as KtClass)
                }
                is org.jetbrains.kotlin.descriptors.TypeAliasDescriptor -> {
                    val kType = descriptor.expandedType
                    KtTypeAliasCompletionDecorator(lookup, psi as KtTypeAlias, kType)
                }
                else -> null
            }
        }
    }
}

// Java

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


// Kotlin

class KtClassCompletionDecorator(lookup: LookupElement, val klass: KtClass)
    : CompletionDecorator<KtClass>(lookup, klass) {
    override val kind = CompletionItemKind.Class

    override fun formatLabel() = klass.fqName?.asString() ?: klass.toString()
}

class KtPropertyCompletionDecorator(lookup: LookupElement, val property: KtProperty, val type: KotlinType)
    : CompletionDecorator<KtProperty>(lookup, property) {
    override val kind = if(property.isMember) CompletionItemKind.Field else CompletionItemKind.Variable

    override fun formatLabel() = "${property.name} : $type"

    override fun formatDoc(): String = "$type ${property.name};"
}

class KtVariableCompletionDecorator(lookup: LookupElement, val property: KtProperty, val type: KotlinType)
    : CompletionDecorator<KtProperty>(lookup, property) {
    override val kind = if(property.isMember) CompletionItemKind.Field else CompletionItemKind.Variable

    override fun formatLabel() = "${property.name} : $type"

    override fun formatDoc(): String = "${property.name}: $type"
}

class KtFunctionCompletionDecorator(lookup: LookupElement, val function: KtNamedFunction, val type: KotlinType?, val params: List<ValueParameterDescriptor>)
    : CompletionDecorator<KtNamedFunction>(lookup, function) {
    override val kind = CompletionItemKind.Function

    val argsString = params.joinToString(", ") { "${it.name.asString()}: ${it.type}" }

    override fun formatLabel() = "${function.name}($argsString) : $type"

    override fun formatDoc() = formatLabel()

    override fun formatInsertText() =
        super.formatInsertText() + buildMethodParams(params, clientSupportsSnippets)
}

class KtTypeAliasCompletionDecorator(lookup: LookupElement, val typeAlias: KtTypeAlias, val type: SimpleType)
    : CompletionDecorator<KtTypeAlias>(lookup, typeAlias) {
    override val kind = CompletionItemKind.Class

    override fun formatLabel() = "${typeAlias.name} : $type"
}


fun buildDocComment(method: PsiElement): String {
    val docComment = (method as? PsiDocCommentOwner)?.docComment ?: return ""

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

fun buildMethodParams(params: List<ValueParameterDescriptor>, snippetSupport: Boolean) = if (snippetSupport)
    buildSnippetTabStops(params)
else
    buildParens(params)

fun buildParens(method: PsiMethod) = if (method.parameters.isEmpty()) "()" else "("
fun buildParens(params: List<ValueParameterDescriptor>) = if (params.isEmpty()) "()" else "("

fun buildSnippetTabStops(method: PsiMethod): CharSequence {
    val tabStops = method.parameterList.parameters.mapIndexed(::methodParamToSnippet).joinToString(", ")

    return "($tabStops)$0"
}
fun buildSnippetTabStops(parameters: List<ValueParameterDescriptor>): CharSequence {
    val tabStops = parameters.mapIndexed(::methodParamToSnippet).joinToString(", ")

    return "($tabStops)$0"
}

fun methodParamToSnippet(index: Int, param: PsiParameter) =
    "${'$'}${'{'}${index+1}:${param.name}${'}'}"
fun methodParamToSnippet(index: Int, param: ValueParameterDescriptor) =
    "${'$'}${'{'}${index+1}:${param.name}${'}'}"

fun getTypeName(type: PsiType?) =
    type?.presentableText ?: "void"

