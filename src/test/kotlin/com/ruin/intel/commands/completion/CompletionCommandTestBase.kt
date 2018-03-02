package com.ruin.intel.commands.completion

import com.ruin.intel.DUMMY_FILE_PATH
import com.ruin.intel.JAVA_PROJECT
import com.ruin.intel.model.execute
import com.ruin.intel.values.Position
import com.ruin.intel.values.TextDocumentIdentifier
import org.intellivim.FileEditingTestCase

abstract class CompletionCommandTestBase : FileEditingTestCase() {
    override val filePath: String
        get() = DUMMY_FILE_PATH

    override val projectName: String
        get() = JAVA_PROJECT

    protected fun checkContainsCompletion(line: Int, char: Int, snippet: Boolean, expected: String) {
        val command = CompletionCommand(TextDocumentIdentifier(file.url), Position(line, char),
            null, null, snippet)
        val result = execute(command, file.url)
        assertTrue("Expected $expected to be included but got: \n${result.items}",
            result.items.any { it.label == expected || it.insertText == expected })
        command.dispose()
    }
}
