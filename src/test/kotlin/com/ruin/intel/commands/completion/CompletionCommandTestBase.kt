package com.ruin.intel.commands.completion

import com.ruin.intel.DUMMY_FILE_PATH
import com.ruin.intel.JAVA_PROJECT
import com.ruin.intel.commands.CompletionCommand
import com.ruin.intel.values.Position
import com.ruin.intel.values.TextDocumentIdentifier
import org.intellivim.FileEditingTestCase

abstract class CompletionCommandTestBase : FileEditingTestCase() {
    override val filePath: String
        get() = DUMMY_FILE_PATH

    override fun getProjectName() = JAVA_PROJECT

    protected fun checkContainsCompletion(line: Int, char: Int, expected: String) {
        val command = CompletionCommand(TextDocumentIdentifier(file.url), Position(line, char), null, null)
        val result = command.execute()
        assertNull(result.component2())
        assertTrue("Expected $expected to be included but got: \n${result.get().map{ it.label }}",
            result.get().any { it.label == expected })
        command.dispose()
    }
}
