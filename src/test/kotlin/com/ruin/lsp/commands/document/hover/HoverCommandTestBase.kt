package com.ruin.lsp.commands.document.hover

import com.intellij.openapi.project.DumbService
import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.model.invokeCommandAndWait
import org.eclipse.lsp4j.Position
import org.intellivim.FileEditingTestCase

abstract class HoverCommandTestBase : FileEditingTestCase() {
    protected fun checkHoverEquals(line: Int, char: Int, expected: String?) {
        DumbService.getInstance(project).runWhenSmart {
            val command = HoverCommand(Position(line, char))
            val result = invokeCommandAndWait(command, file.url)
            val value = result.contents.first().right.value
            assertEquals("Expected \"$expected\" but got: \n$value",
                expected, value)
        }
    }

    protected fun checkHoverIsEmpty(line: Int, char: Int) {
        DumbService.getInstance(project).runWhenSmart {
            val command = HoverCommand(Position(line, char))
            val result = invokeCommandAndWait(command, file.url)
            assertEmpty(result.contents)
        }
    }
}
