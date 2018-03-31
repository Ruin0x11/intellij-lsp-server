package com.ruin.lsp.commands.document.lens

import com.intellij.codeInsight.TestFrameworks
import com.intellij.codeInsight.daemon.impl.Divider
import com.intellij.codeInsight.daemon.impl.LineMarkersPass
import com.intellij.execution.ExecutorRegistry
import com.intellij.execution.actions.ConfigurationContext
import com.intellij.execution.lineMarker.ExecutorAction
import com.intellij.execution.lineMarker.LineMarkerActionWrapper
import com.intellij.openapi.actionSystem.ActionGroup
import com.intellij.openapi.actionSystem.ActionManager
import com.intellij.openapi.actionSystem.DefaultActionGroup
import com.intellij.openapi.editor.Document
import com.intellij.psi.*
import com.intellij.psi.util.ClassUtil
import com.intellij.psi.util.PsiMethodUtil
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.util.containers.mapSmart
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.util.getDocument
import com.ruin.lsp.util.toRange
import org.eclipse.lsp4j.CodeLens
import org.eclipse.lsp4j.Command

class CodeLensCommand : DocumentCommand<MutableList<CodeLens>> {
    override fun execute(ctx: ExecutionContext): MutableList<CodeLens> {
        val doc = getDocument(ctx.file) ?: return mutableListOf()

        val isFile = { file: PsiFile -> file === ctx.file }

        var lineMarkers: List<CodeLens> = listOf()
        Divider.divideInsideAndOutsideAllRoots(ctx.file, ctx.file.textRange, ctx.file.textRange, isFile) { elements ->
            lineMarkers = elements.inside.mapNotNull { it.codeLens(ctx.file, doc) }
            true
        }

        val asd = LineMarkersPass.queryLineMarkers(ctx.file, doc)

        return lineMarkers.toMutableList()
    }
}

/* copied from RunLineMarkerProvider but modified so actions aren't wrapped (they would become private) */
private fun PsiElement.codeLens(file: PsiFile, doc: Document): CodeLens? {
    val markers = LineMarkersPass.queryLineMarkers(file, doc)

    val runAction = markers.find {
        (it.createGutterRenderer()?.popupMenuActions as? DefaultActionGroup)?.childActionsOrStubs?.any { a -> a is LineMarkerActionWrapper } == true
    } ?: return null

    // We know the run action happens for this element, so we should be able to create a ConfigurationContext from it.
    val configContext = ConfigurationContext(runAction.element)

    return CodeLens().apply {
        this.range = range
        this.command = Command("Run Line", "runLine")
    }
}
