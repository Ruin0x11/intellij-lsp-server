package com.ruin.lsp.commands.document.completion;

import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase
import com.ruin.lsp.model.invokeCommandAndWait
import com.ruin.lsp.util.position
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.TextEdit

abstract class CompletionItemResolveCommandTestBase : LightCodeInsightFixtureTestCase() {
    private fun runCommand(pos: Position, selectedItem: String): CompletionItem {
        val completionCommand = CompletionCommand(pos, false)
        val completionResult = invokeCommandAndWait(completionCommand, project, file)
        val itemToImport: CompletionItem? = completionResult.right.items.find { it.label == selectedItem }
        assertNotNull("Item $selectedItem not in completion results: ${completionResult.right.items}", itemToImport)
        val command = CompletionItemResolveCommand(itemToImport!!)
        return invokeCommandAndWait(command, project, file)
    }

    protected fun checkHasAdditionalEdits(selectedItem: String, edits: List<TextEdit>) {
        val result = runCommand(position(), selectedItem)

        assertSameElements(edits, result.additionalTextEdits)
    }

    private fun position() =
        myFixture.editor.caretModel.currentCaret.logicalPosition.position()
}
