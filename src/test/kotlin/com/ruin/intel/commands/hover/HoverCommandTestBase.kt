package com.ruin.intel.commands.hover

import com.ruin.intel.BaseTestCase
import com.ruin.intel.DUMMY_FILE_PATH
import com.ruin.intel.JAVA_PROJECT
import com.ruin.intel.commands.HoverCommand
import com.ruin.intel.values.Position
import com.ruin.intel.values.TextDocumentIdentifier
import org.intellivim.FileEditingTestCase

open class HoverCommandTestBase : FileEditingTestCase() {
    override fun getProjectName() = JAVA_PROJECT

    override val filePath: String
        get() = DUMMY_FILE_PATH

    protected fun checkHoverContains(line: Int, char: Int, expected: String) {
        val command = HoverCommand(TextDocumentIdentifier(file.url), Position(line, char))
        val result = command.execute()
        assertNull(result.component2())
        assertTrue("Expected $expected to be included but got: \n${result.get()}",
            result.get().contains(expected))
        command.dispose()
    }
}
