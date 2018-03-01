package com.ruin.intel.commands.hover

import com.intellij.codeInsight.CodeInsightBundle
import com.intellij.codeInsight.completion.CompletionMemory
import com.intellij.codeInsight.javadoc.JavaDocExternalFilter
import com.intellij.codeInsight.javadoc.JavaDocInfoGenerator
import com.intellij.lang.java.JavaDocumentationProvider
import com.intellij.psi.*
import com.intellij.psi.util.PsiFormatUtil
import com.intellij.psi.util.PsiFormatUtilBase
import com.intellij.psi.util.PsiTreeUtil

class HoverDocumentationProvider : JavaDocumentationProvider() {
    override fun generateDoc(element: PsiElement?, originalElement: PsiElement?): String? {
        var anElement = element
        var anOriginalElement = originalElement
        // for new Class(<caret>) or methodCall(<caret>) proceed from method call or new expression
        // same for new Cl<caret>ass() or method<caret>Call()
        if (anElement is PsiExpressionList || anElement is PsiReferenceExpression && anElement.getParent() is PsiMethodCallExpression) {
            anElement = anElement.parent
            anOriginalElement = null
        }
        if (anElement is PsiMethodCallExpression) {
            val method = CompletionMemory.getChosenMethod(anElement)
            if (method == null)
                return getMethodCandidateInfo(anElement)
            else
                anElement = method
        }

        // Try hard for documentation of incomplete new Class instantiation
        var elt = if (anOriginalElement != null && anOriginalElement !is PsiPackage) PsiTreeUtil.prevLeaf(anOriginalElement) else anElement
        if (elt is PsiErrorElement)
            elt = elt.prevSibling
        else if (elt != null && elt !is PsiNewExpression) {
            elt = elt.parent
        }
        if (elt is PsiNewExpression) {
            var targetClass: PsiClass? = null

            if (anElement is PsiJavaCodeReferenceElement) {     // new Class<caret>
                val resolve = anElement.resolve()
                if (resolve is PsiClass) targetClass = resolve
            } else if (anElement is PsiClass) { //Class in completion
                targetClass = anElement
            } else if (anElement is PsiNewExpression) { // new Class(<caret>)
                val reference = anElement.classReference
                if (reference != null) {
                    val resolve = reference.resolve()
                    if (resolve is PsiClass) targetClass = resolve
                }
            }

            if (targetClass != null) {
                val constructors = targetClass.constructors
                if (constructors.isNotEmpty()) {
                    if (constructors.size == 1) return generateDoc(constructors[0], anOriginalElement)
                    val sb = StringBuilder()

                    for (constructor in constructors) {
                        val str = PsiFormatUtil.formatMethod(constructor, PsiSubstitutor.EMPTY,
                            PsiFormatUtilBase.SHOW_NAME or
                                PsiFormatUtilBase.SHOW_TYPE or
                                PsiFormatUtilBase.SHOW_PARAMETERS,
                            PsiFormatUtilBase.SHOW_TYPE or PsiFormatUtilBase.SHOW_NAME)
                        sb.append(str)
                    }

                    return CodeInsightBundle.message("javadoc.constructor.candidates", targetClass.name, sb)
                }
            }
        }

        return generateOneLineJavadoc(anElement!!)
    }

    private fun generateOneLineJavadoc(element: PsiElement): String? {
        val docURLs = getExternalJavaDocUrl(element)
        return generateOneLineJavadoc(element, docURLs)
    }

    private fun generateOneLineJavadoc(element: PsiElement, docURLs: List<String>?): String? {
        val javaDocInfoGenerator = OneLineJavaDocInfoGeneratorFactory.create(element.project, element)
        return generateOneLineJavadoc(javaDocInfoGenerator, docURLs)
    }

    private fun generateOneLineJavadoc(generator: JavaDocInfoGenerator, docURLs: List<String>?): String? {
        return JavaDocExternalFilter.filterInternalDocInfo(generator.generateDocInfo(docURLs))
    }

    private fun getMethodCandidateInfo(expr: PsiMethodCallExpression): String? {
        val rh = JavaPsiFacade.getInstance(expr.project).resolveHelper
        val candidates = rh.getReferencedMethodCandidates(expr, true)
        val text = expr.text
        if (candidates.isNotEmpty()) {
            if (candidates.size == 1) {
                val element = candidates[0].element
                if (element is PsiMethod) return generateDoc(element, null)
            }
            val sb = StringBuilder()

            for (candidate in candidates) {
                val element = candidate.element as? PsiMethod ?: continue

                val str = PsiFormatUtil.formatMethod(element, candidate.substitutor,
                    PsiFormatUtilBase.SHOW_NAME or
                        PsiFormatUtilBase.SHOW_TYPE or
                        PsiFormatUtilBase.SHOW_PARAMETERS,
                    PsiFormatUtilBase.SHOW_TYPE)
                sb.append(str)
            }

            return CodeInsightBundle.message("javadoc.candidates", text, sb)
        }

        return CodeInsightBundle.message("javadoc.candidates.not.found", text)
    }
}
