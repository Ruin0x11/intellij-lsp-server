package com.ruin.lsp.commands.document.completion

import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.*
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.CompletionItemKind
import org.eclipse.lsp4j.InsertTextFormat
import org.jetbrains.kotlin.builtins.extractParameterNameFromFunctionTypeArgument
import org.jetbrains.kotlin.builtins.isBuiltinFunctionalType
import org.jetbrains.kotlin.descriptors.ValueParameterDescriptor
import org.jetbrains.kotlin.idea.completion.BasicLookupElementFactory
import org.jetbrains.kotlin.idea.completion.DeclarationLookupObjectImpl
import org.jetbrains.kotlin.idea.completion.KeywordLookupObject
import org.jetbrains.kotlin.idea.completion.LambdaSignatureTemplates
import org.jetbrains.kotlin.idea.core.KotlinNameSuggester
import org.jetbrains.kotlin.idea.core.completion.DeclarationLookupObject
import org.jetbrains.kotlin.load.java.descriptors.JavaClassDescriptor
import org.jetbrains.kotlin.load.java.descriptors.JavaMethodDescriptor
import org.jetbrains.kotlin.load.java.descriptors.JavaPropertyDescriptor
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.psi.psiUtil.quoteIfNeeded
import org.jetbrains.kotlin.renderer.render
import org.jetbrains.kotlin.resolve.calls.util.getValueParametersCountFromFunctionType
import org.jetbrains.kotlin.resolve.descriptorUtil.classValueType
import org.jetbrains.kotlin.resolve.descriptorUtil.fqNameSafe
import org.jetbrains.kotlin.serialization.deserialization.descriptors.DeserializedPropertyDescriptor
import org.jetbrains.kotlin.serialization.deserialization.descriptors.DeserializedSimpleFunctionDescriptor
import org.jetbrains.kotlin.synthetic.SamAdapterExtensionFunctionDescriptor
import org.jetbrains.kotlin.synthetic.SyntheticJavaPropertyDescriptor
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.types.SimpleType


abstract class CompletionDecorator<out T : Any>(val lookup: LookupElement, val elt: T) {
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
        fun from(lookup: LookupElement, snippetSupport: Boolean): CompletionDecorator<Any>? {
            val psi = lookup.psiElement

            if (lookup.`object` is String || lookup.`object` is KeywordLookupObject) {
                return KtKeywordCompletionDecorator(lookup)
            }

            // handle generated properties and language builtin methods
            var decorator = fromSyntheticLookupElement(lookup)

            if (decorator == null) {
                decorator = when (psi) {
                    is PsiMethod -> MethodCompletionDecorator(lookup, psi)
                    is PsiClass -> ClassCompletionDecorator(lookup, psi)
                    is PsiField -> FieldCompletionDecorator(lookup, psi)
                    is PsiVariable -> VariableCompletionDecorator(lookup, psi)
                    is PsiPackage -> PackageCompletionDecorator(lookup, psi)

                    is KtElement -> fromKotlin(lookup)
                    else -> null
                }
            }
            decorator?.clientSupportsSnippets = snippetSupport
            return decorator
        }

        fun fromKotlin(lookup: LookupElement): CompletionDecorator<Any>? {
            val descriptor = (lookup.`object` as? DeclarationLookupObjectImpl)?.descriptor ?: return null
            val psi = lookup.psiElement
            return when (descriptor) {
                is org.jetbrains.kotlin.descriptors.ValueParameterDescriptor -> {
                    KtValueParameterCompletionDecorator(lookup, psi as KtParameter)
                }
                is org.jetbrains.kotlin.descriptors.VariableDescriptor -> {
                    val kType = descriptor.type

                    when (psi) {
                        is KtProperty -> {
                            val name = psi.nameAsSafeName
                            val isMember = psi.isMember
                            KtVariableCompletionDecorator(lookup, name, kType, isMember)
                        }
                        is KtDestructuringDeclarationEntry -> null
                        else -> null
                    }
                }
                is org.jetbrains.kotlin.descriptors.PropertyDescriptor -> {
                    val kType = descriptor.returnType ?: descriptor.type
                    KtPropertyCompletionDecorator(lookup, psi as KtProperty, kType)
                }
                is org.jetbrains.kotlin.descriptors.FunctionDescriptor -> {
                    if (psi is KtNamedFunction) {
                        val name = descriptor.name
                        val kType = descriptor.returnType
                        val args = descriptor.valueParameters
                        val hasSynthesizedParameterNames = descriptor.hasSynthesizedParameterNames()
                        KtFunctionCompletionDecorator(lookup, name, kType, args, hasSynthesizedParameterNames)
                    } else {
                        null
                    }
                }
                is org.jetbrains.kotlin.descriptors.ClassDescriptor -> {
                    val kType = descriptor.classValueType ?: descriptor.defaultType

                    when (psi) {
                        is KtClass -> KtClassCompletionDecorator(lookup, psi.nameAsSafeName, psi.fqName)
                        is KtObjectDeclaration -> KtObjectCompletionDecorator(lookup, psi)
                        else -> null
                    }
                }
                is org.jetbrains.kotlin.descriptors.TypeAliasDescriptor -> {
                    val kType = descriptor.expandedType
                    KtTypeAliasCompletionDecorator(lookup, psi as KtTypeAlias, kType)
                }
                else -> null
            }
        }

        private fun fromSyntheticLookupElement(lookup: LookupElement): CompletionDecorator<Any>? {
            val obj = lookup.`object`
            return if (obj is DeclarationLookupObject) {
                val descriptor = obj.descriptor
                when (descriptor) {
                    // value from getValue()/setValue(), size from getSize()
                    is SyntheticJavaPropertyDescriptor, is DeserializedPropertyDescriptor -> if (lookup.psiElement is PsiMethod) {
                        KtSyntheticPropertyCompletionDecorator(lookup, lookup.psiElement as PsiMethod, descriptor.name)
                    } else {
                        null
                    }
                    // joinToString, first, last, contains...
                    is DeserializedSimpleFunctionDescriptor -> {
                        val name = descriptor.name
                        val kType = descriptor.returnType
                        val args = descriptor.valueParameters
                        val hasSynthesizedParameterNames = descriptor.hasSynthesizedParameterNames()
                        KtFunctionCompletionDecorator(lookup, name, kType, args, hasSynthesizedParameterNames)
                    }
                    // forEach
                    is SamAdapterExtensionFunctionDescriptor -> {
                        val name = descriptor.name
                        val kType = descriptor.returnType
                        val args = descriptor.valueParameters
                        val hasSynthesizedParameterNames = descriptor.hasSynthesizedParameterNames()
                        KtFunctionCompletionDecorator(lookup, name, kType, args, hasSynthesizedParameterNames)
                    }
                    // stream
                    is JavaMethodDescriptor -> {
                        val name = descriptor.name
                        val kType = descriptor.returnType
                        val args = descriptor.valueParameters
                        val hasSynthesizedParameterNames = descriptor.hasSynthesizedParameterNames()
                        KtFunctionCompletionDecorator(lookup, name, kType, args, hasSynthesizedParameterNames)
                    }
                    // values from getValues()
                    is JavaPropertyDescriptor -> {
                        val name = descriptor.name
                        val kType = descriptor.returnType
                        val isMember = true
                        KtVariableCompletionDecorator(lookup, name, kType, isMember)
                    }
                    is JavaClassDescriptor -> {
                        val name = descriptor.name
                        val fqName = descriptor.fqNameSafe
                        KtClassCompletionDecorator(lookup, name, fqName)
                    }
                    else -> null
                }
            } else {
                null
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
}

class PackageCompletionDecorator(lookup: LookupElement, val pack: PsiPackage)
    : CompletionDecorator<PsiPackage>(lookup, pack) {
    override val kind = CompletionItemKind.Module

    override fun formatLabel() = pack.qualifiedName
}


// Kotlin

class KtClassCompletionDecorator(lookup: LookupElement, val name: Name, val fqName: FqName?)
    : CompletionDecorator<Name>(lookup, name) {
    override val kind = CompletionItemKind.Class

    override fun formatLabel() = fqName?.asString() ?: name.asString()
}

class KtPropertyCompletionDecorator(lookup: LookupElement, val property: KtProperty, val type: KotlinType)
    : CompletionDecorator<KtProperty>(lookup, property) {
    override val kind = if(property.isMember) CompletionItemKind.Field else CompletionItemKind.Variable

    override fun formatLabel() = "${property.name} : $type"
}

class KtVariableCompletionDecorator(lookup: LookupElement, val name: Name, val type: KotlinType, val isMember: Boolean)
    : CompletionDecorator<Name>(lookup, name) {
    override val kind = if(isMember) CompletionItemKind.Field else CompletionItemKind.Variable

    override fun formatLabel() = "${name} : $type"
}

class KtFunctionCompletionDecorator(lookup: LookupElement,
                                    val name: Name,
                                    val returnType: KotlinType?,
                                    val args: List<ValueParameterDescriptor>,
                                    val hasSynthesizedParameterNames: Boolean)
    : CompletionDecorator<Name>(lookup, name) {
    override val kind = CompletionItemKind.Function
    private val parametersRenderer = BasicLookupElementFactory.SHORT_NAMES_RENDERER

    private fun argsString(args: List<ValueParameterDescriptor>) =
        if (isRenderableAsLambda() && args.isEmpty()) {
            ""
        } else {
            parametersRenderer.renderValueParameters(args, hasSynthesizedParameterNames)
        }

    private fun lastParameterType() = args.lastOrNull()?.original?.type
    private fun isRenderableAsLambda() = lastParameterType()?.isBuiltinFunctionalType == true

    private fun renderAsLambda(): String {
        val parameterType = lastParameterType()!!
        val explicitLambdaParameters = getValueParametersCountFromFunctionType(parameterType) > 1
        val lambdaPresentation = if (explicitLambdaParameters)
            LambdaSignatureTemplates.lambdaPresentation(parameterType, LambdaSignatureTemplates.SignaturePresentation.NAMES_OR_TYPES)
        else
            LambdaSignatureTemplates.DEFAULT_LAMBDA_PRESENTATION

        return "$name${argsString(args.dropLast(1))} $lambdaPresentation : $returnType"
    }

    override fun formatLabel() =
        if (isRenderableAsLambda()) {
            renderAsLambda()
        } else {
            "$name${argsString(args)} : $returnType"
        }

    override fun formatInsertText() =
        if (isRenderableAsLambda()) {
            super.formatInsertText() + buildLambdaMethodParams(args, clientSupportsSnippets)
        } else {
            super.formatInsertText() + buildMethodParams(args, clientSupportsSnippets)
        }
}

class KtTypeAliasCompletionDecorator(lookup: LookupElement, val typeAlias: KtTypeAlias, val type: SimpleType)
    : CompletionDecorator<KtTypeAlias>(lookup, typeAlias) {
    override val kind = CompletionItemKind.Class

    override fun formatLabel() = "${typeAlias.name} : $type"
}

class KtValueParameterCompletionDecorator(lookup: LookupElement, val valueParameter: KtParameter)
    : CompletionDecorator<KtParameter>(lookup, valueParameter) {
    override val kind = CompletionItemKind.Field
    private val type = valueParameter.typeReference?.typeElement?.text!!

    override fun formatLabel() = "${valueParameter.name} : $type"
}

class KtObjectCompletionDecorator(lookup: LookupElement, val obj: KtObjectDeclaration)
    : CompletionDecorator<KtObjectDeclaration>(lookup, obj){
    override val kind = CompletionItemKind.Class

    override fun formatLabel() = "${obj.name} : object"
}

class KtSyntheticPropertyCompletionDecorator(lookup: LookupElement, val method: PsiMethod, val realName: Name)
    : CompletionDecorator<PsiMethod>(lookup, method) {
    override val kind: CompletionItemKind
        get() = CompletionItemKind.Property

    override fun formatLabel() = "$realName (from ${method.name}${buildParamsList(method)}) : ${getTypeName(method.returnType)}"

    override fun formatInsertText() = realName.asString().quoteIfNeeded()
}

class KtKeywordCompletionDecorator(lookup: LookupElement)
    : CompletionDecorator<String>(lookup, lookup.lookupString) {
    override val kind: CompletionItemKind
        get() = CompletionItemKind.Keyword

    override fun formatLabel() = lookup.lookupString

    override fun formatInsertText() = lookup.lookupString
}


fun buildDocComment(method: Any): String {
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

private fun buildMethodParams(method: PsiMethod, snippetSupport: Boolean) = if (snippetSupport)
    buildSnippetTabStops(method)
else
    buildParens(method)

private fun buildMethodParams(params: List<ValueParameterDescriptor>, snippetSupport: Boolean) = if (snippetSupport)
    buildSnippetTabStops(params)
else
    buildParens(params)

private fun buildLambdaMethodParams(params: List<ValueParameterDescriptor>, snippetSupport: Boolean) = if (snippetSupport)
    buildLambdaSnippetTabStops(params)
else
    buildLambdaParens(params)

private fun buildParens(method: PsiMethod) = if (method.parameters.isEmpty()) "()" else "("
private fun buildParens(parameters: List<ValueParameterDescriptor>) = if (parameters.isEmpty()) "()" else "("
private fun buildLambdaParens(parameters: List<ValueParameterDescriptor>): CharSequence {
    if (parameters.size != 1) {
        return buildParens(parameters)
    }

    val lastParameter = parameters.last()
    val parameterType = lastParameter.original.type
    assert(parameterType.isBuiltinFunctionalType)
    val explicitLambdaParameters = getValueParametersCountFromFunctionType(parameterType) > 1

    return if (explicitLambdaParameters) {
        val args = parameterType.arguments.dropLast(1) // last param is return type
            .mapIndexed { i, it -> getNameForType(it.type) }
            .joinToString(", ")
        " { $args -> "
    } else {
        " { "
    }
}

private fun buildSnippetTabStops(method: PsiMethod): CharSequence {
    val tabStops = method.parameterList.parameters.mapIndexed(::methodParamToSnippet).joinToString(", ")

    return "($tabStops)$0"
}
private fun buildSnippetTabStops(parameters: List<ValueParameterDescriptor>): CharSequence {
    val tabStops = parameters.mapIndexed(::methodParamToSnippet).joinToString(", ")

    return "($tabStops)$0"
}

private fun buildLambdaSnippetTabStops(parameters: List<ValueParameterDescriptor>): CharSequence {
    val preceedingTabStops = if (parameters.size > 1) {
        parameters.dropLast(1)
            .mapIndexed(::methodParamToSnippet)
            .joinToString(", ")
            .let { "($it) "}
    } else {
        " "
    }

    val lastParameter = parameters.last()
    val parameterType = lastParameter.original.type
    assert(parameterType.isBuiltinFunctionalType)
    val explicitLambdaParameters = getValueParametersCountFromFunctionType(parameterType) > 1
    val lastIdx = parameters.size - 1

    return if (explicitLambdaParameters) {
        val args = parameterType.arguments.dropLast(1) // last param is return type
            .mapIndexed { i, it -> methodParamToSnippet(lastIdx + i, getNameForType(it.type)) }
            .joinToString(", ")
        "$preceedingTabStops{ $args -> $0 }"
    } else {
        "$preceedingTabStops{ $0 }"
    }
}

private fun methodParamToSnippet(index: Int, param: PsiParameter) =
    methodParamToSnippet(index, param.name ?: "<unknown>")
private fun methodParamToSnippet(index: Int, param: ValueParameterDescriptor) =
    methodParamToSnippet(index, param.name.asString())
private fun methodParamToSnippet(index: Int, name: String) =
    "${'$'}${'{'}${index+1}:$name${'}'}"

private fun getTypeName(type: PsiType?) =
    type?.presentableText ?: "void"

private fun getNameForType(parameterType: KotlinType) =
    parameterType.extractParameterNameFromFunctionTypeArgument()?.render()
        ?: KotlinNameSuggester.suggestNamesByType(parameterType, { true }, "p")[0]
