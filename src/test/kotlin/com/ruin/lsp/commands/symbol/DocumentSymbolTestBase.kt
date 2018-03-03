package com.ruin.lsp.commands.symbol

import com.ruin.lsp.BaseTestCase
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.model.execute
import com.ruin.lsp.util.getVirtualFile

abstract class DocumentSymbolTestBase : BaseTestCase() {

    override val projectName: String
        get() = JAVA_PROJECT

    protected fun checkFindsSymbol(filePath: String, expectedName: String, expectedKind: Int) {
        val file = getVirtualFile(project, filePath)
        val command = DocumentSymbolCommand()
        val result = execute(command, file.url)
        assertTrue("Expected $expectedName: $expectedKind to be found but got: \n$result",
            result.any { it.name == expectedName || it.kind == expectedKind })
    }
}
