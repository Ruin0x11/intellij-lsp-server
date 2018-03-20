package com.ruin.lsp.commands.document.completion;

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

    private fun runCommand(pos: Position, selectedItem: String): CompletionItem {
        val completionCommand = CompletionCommand(pos, false)
        val file = getVirtualFile(proj!!, DUMMY_FILE_PATH)
        val completionResult = invokeCommandAndWait(completionCommand, file.url)
        val itemToImport: CompletionItem? = completionResult.right.items.find { it.label == selectedItem }
        assertNotNull(itemToImport, "Item $selectedItem not in completion results: ${completionResult.right.items}")
        val command = CompletionItemResolveCommand(itemToImport!!)
        return invokeCommandAndWait(command, file.url)
    }

    override fun tearDown() {
        ProjectUtil.closeAndDispose(proj!!)
        super.tearDown()
    }

    override fun setUp() {
        super.setUp()
        proj = prepareProject(JAVA_PROJECT)
    }

    protected fun checkHasAdditionalEdit(pos: Position, selectedItem: String, edit: TextEdit) {
        val result = runCommand(pos, selectedItem)

        // There will be a dummy identifier ("IntellijIdeaRulezzz") inserted as part of the completion, so just ignore
        // it and make sure the change we want is present.
        assert(result.additionalTextEdits.contains(edit), { "Wanted: $edit\nGot: ${result.additionalTextEdits}" })
    }

    protected fun checkHasNoAdditionalEdits(pos: Position, selectedItem: String) {
        val result = runCommand(pos, selectedItem)

        // There will be a dummy identifier ("IntellijIdeaRulezzz") inserted as part of the completion, so ensure
        // it is the only element in the results
        assert(result.additionalTextEdits.size == 1, { "Got: ${result.additionalTextEdits}" })
    }
}
