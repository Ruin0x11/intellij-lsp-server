package com.ruin.lsp.commands.document.completion;

import com.intellij.ide.impl.ProjectUtil
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
    protected fun checkHasAdditionalEdit(pos: Position, selectedItem: String, edit: TextEdit) {
        val completionCommand = CompletionCommand(pos, false)
        val project = prepareProject(JAVA_PROJECT)
        val file = getVirtualFile(project, DUMMY_FILE_PATH)
        val completionResult = invokeCommandAndWait(completionCommand, file.url)
        val itemToImport: CompletionItem? = completionResult.right.items.find { it.label == selectedItem }
        assertNotNull(itemToImport, "Item $selectedItem not in completion results")
        val command = CompletionItemResolveCommand(itemToImport!!)
        val result = invokeCommandAndWait(command, file.url)

        // There will be a dummy identifier ("IntellijIdeaRulezzz") inserted as part of the completion, so just ignore
        // it and make sure the change we want is present.
        assert(result.additionalTextEdits.contains(edit), { "Wanted: $edit\nGot: ${result.additionalTextEdits}" })
        ProjectUtil.closeAndDispose(project)
    }
}
