package com.ruin.lsp.commands.find

import com.ruin.lsp.BaseTestCase
import com.ruin.lsp.util.getVirtualFile
import com.ruin.lsp.commands.Command
import com.ruin.lsp.model.invokeCommandAndWait
import com.ruin.lsp.util.ensurePsiFromUri
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.Position

abstract class FindCommandTestBase : BaseTestCase() {
    abstract fun command(at: Position, uri: String): Command<MutableList<Location>>
    private fun resolveCommand(at: Position, filePath: String): Pair<Command<MutableList<Location>>, String> {
        val file = getVirtualFile(project, filePath)
        return Pair(command(at, file.url), file.url)
    }

    private fun checkFindsLocation(command: Command<MutableList<Location>>,
                                     uri: String,
                                     expectedFile: String,
                                     expectedPos: Position) {
        val result = invokeCommandAndWait(command, uri)
        assertTrue("Expected ($expectedFile, $expectedPos to be included in results but got: " +
            "\n$result",
            result.any {
                extractFileName(it.uri) == expectedFile &&
                    it.range.start == expectedPos
            })
    }

    protected fun checkFindsLocation(filePath: String, at: Position, expectedFile: String, expectedPos: Position) {
        val (command, uri) = resolveCommand(at, filePath)
        checkFindsLocation(command, uri, expectedFile, expectedPos)
    }

    private fun checkFindsNothing(command: Command<MutableList<Location>>,
                                    uri: String) {
        val result = invokeCommandAndWait(command, uri)
        assertTrue("Expected nothing to be found but got: \n$result", result.isEmpty())
    }

    protected fun checkFindsNothing(filePath: String, at: Position) {
        val (command, uri) = resolveCommand(at, filePath)
        checkFindsNothing(command, uri)
    }
}

fun extractFileName(uri: String) = uri.substring(uri.lastIndexOf("/") + 1)
