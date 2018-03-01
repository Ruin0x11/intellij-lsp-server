package com.ruin.intel.commands.definition

import com.ruin.intel.BaseTestCase
import com.ruin.intel.DUMMY_FILE_PATH
import com.ruin.intel.JAVA_PROJECT
import com.ruin.intel.Util.getVirtualFile
import com.ruin.intel.values.Location
import com.ruin.intel.values.Position
import com.ruin.intel.values.TextDocumentIdentifier

abstract class DefinitionCommandTestBase : BaseTestCase() {
    override val projectName: String
        get() = JAVA_PROJECT

    protected fun checkFindsDefinition(filePath: String, line: Int, char: Int, expectedFile: String, expectedPos: Position) {
        val file = getVirtualFile(project, filePath)
        val command = DefinitionCommand(TextDocumentIdentifier(file.url), Position(line, char))
        val result = command.execute()
        if (result.component2() != null) {
            throw result.component2()!!
        }
        val location = result.get()
        assertTrue("Expected ($expectedFile, $expectedPos to be included in results but got: " +
            "\n${location}",
                extractFileName(location.uri) == expectedFile &&
                    location.range.start == expectedPos)
        command.dispose()
    }
}

fun extractFileName(uri: String) = uri.substring(uri.lastIndexOf("/") + 1)
