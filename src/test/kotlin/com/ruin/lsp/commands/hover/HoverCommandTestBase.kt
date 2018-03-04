package com.ruin.lsp.commands.hover

import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.model.execute
import com.ruin.lsp.values.Position
import com.ruin.lsp.values.TextDocumentIdentifier
import org.intellivim.FileEditingTestCase

abstract class HoverCommandTestBase : FileEditingTestCase() {
    override val projectName = JAVA_PROJECT

    override val filePath = DUMMY_FILE_PATH

    protected fun checkHoverEquals(line: Int, char: Int, expected: String?) {
        val command = HoverCommand(TextDocumentIdentifier(file.url), Position(line, char))
        val result = execute(command, file.url)
        assertEquals("Expected \"$expected\" but got: \n$result",
            expected, result.value)
    }
}
