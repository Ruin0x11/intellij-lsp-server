package com.ruin.lsp.commands.document.completion;

import com.intellij.codeInsight.completion.CompletionUtilCore
import com.intellij.ide.impl.ProjectUtil
import com.intellij.openapi.project.Project
import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.ProjectUtilMavenMultiModuleTest
import com.ruin.lsp.UsableSdkTestCase
import com.ruin.lsp.model.invokeCommandAndWait
import com.ruin.lsp.util.getVirtualFile
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.TextEdit
import kotlin.test.assertNotNull

abstract class CompletionItemResolveCommandTestBase : UsableSdkTestCase() {
    var proj: Project? = null

    override fun setUp() {
        super.setUp()
        proj = prepareProject(JAVA_PROJECT)
    }

    override fun tearDown() {
        ProjectUtil.closeAndDispose(proj!!)
        super.tearDown()
    }

    private fun runCommand(pos: Position, selectedItem: String): CompletionItem {
        val completionCommand = CompletionCommand(pos, false)
        val file = getVirtualFile(proj!!, DUMMY_FILE_PATH)
        val completionResult = invokeCommandAndWait(completionCommand, file.url)
        val itemToImport: CompletionItem? = completionResult.right.items.find { it.label == selectedItem }
        assertNotNull(itemToImport, "Item $selectedItem not in completion results: ${completionResult.right.items}")
        val command = CompletionItemResolveCommand(itemToImport!!)
        return invokeCommandAndWait(command, file.url)
    }

    protected fun checkHasAdditionalEdits(pos: Position, selectedItem: String, edits: List<TextEdit>) {
        val result = runCommand(pos, selectedItem)

        assertSameElements(edits, result.additionalTextEdits)
    }
}
