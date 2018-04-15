package com.ruin.lsp.commands.document.completion

import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.model.invokeCommandAndWait
import org.eclipse.lsp4j.InsertTextFormat
import org.eclipse.lsp4j.Position
import org.intellivim.FileEditingTestCase

abstract class CompletionCommandTestBase : FileEditingTestCase() {
    protected fun checkContainsCompletion(pos: Position, snippet: Boolean, expected: String, expectedInsert: String) {
        val command = CompletionCommand(pos, snippet)
        val result = invokeCommandAndWait(command, file.url)
        assertTrue(result.right.items.all {
            it.insertTextFormat == InsertTextFormat.PlainText ||
                it.insertTextFormat == InsertTextFormat.Snippet
        })
        assertTrue("Expected $expected, $expectedInsert to be included but got: \n${result.right}",
            result.right.items.any { it.label == expected && it.insertText == expectedInsert })
    }
}
