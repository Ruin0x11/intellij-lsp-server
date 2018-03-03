package com.ruin.lsp.commands.symbol

import com.github.kittinunf.result.Result
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.Project
import com.intellij.psi.*
import com.ruin.lsp.commands.Command
import com.ruin.lsp.commands.find.elementToLocation
import com.ruin.lsp.commands.find.elementToLocationWithRange
import com.ruin.lsp.values.SymbolInformation
import com.ruin.lsp.values.SymbolKind

val LOG = Logger.getInstance(DocumentSymbolCommand::class.java)

class DocumentSymbolCommand : Command<List<SymbolInformation>> {
    override fun execute(project: Project, file: PsiFile): Result<List<SymbolInformation>, Exception> {
        val results: MutableList<SymbolInformation> = mutableListOf()

        val visitor = DocumentSymbolGatheringVisitor(results)
        visitor.visitFile(file)

        return Result.of(results)
    }
}

internal class DocumentSymbolGatheringVisitor(val syms: MutableList<SymbolInformation>) : PsiRecursiveElementWalkingVisitor() {
    override fun visitElement(element: PsiElement?) {
        super.visitElement(element)

        if (element != null) {
            val symbolKind = elementToSymbolKind(element) ?: return
            val location = elementToLocationWithRange(element)

            syms.add(SymbolInformation(element.text, symbolKind, location))
        }
    }
}

private fun elementToSymbolKind(element: PsiElement): Int? {
    return when(element) {
        is PsiFile -> SymbolKind.FILE
        is PsiJavaModule -> SymbolKind.MODULE
        is PsiImportStatement -> SymbolKind.PACKAGE
        is PsiEnumConstant -> SymbolKind.ENUM_MEMBER
        is PsiClass -> {
            when {
                element.isEnum -> SymbolKind.ENUM
                element.isInterface -> SymbolKind.INTERFACE
                else -> SymbolKind.CLASS
            }
        }
        is PsiMethod -> {
            when {
                element.isConstructor -> SymbolKind.CONSTRUCTOR
                isStatic(element) -> SymbolKind.FUNCTION
                else -> SymbolKind.METHOD
            }
        }
        is PsiField -> {
            if (isFinal(element))
                SymbolKind.CONSTANT
            else
                SymbolKind.FIELD
        }
        is PsiVariable -> {
            if (isFinal(element))
                SymbolKind.CONSTANT
            else
                SymbolKind.VARIABLE
        }
        is PsiTypeParameter -> SymbolKind.TYPE_PARAMETER
        else -> null
    }
}

private fun hasModifier(element: PsiModifierListOwner, modifier: String) =
    element.modifierList?.hasModifierProperty(modifier) ?: false

private fun isFinal(element: PsiModifierListOwner) = hasModifier(element, PsiModifier.FINAL)
private fun isStatic(element: PsiModifierListOwner)  = hasModifier(element, PsiModifier.STATIC)
