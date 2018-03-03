package com.ruin.lsp.commands.symbol

import com.ruin.lsp.BaseTestCase
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.model.execute
import com.ruin.lsp.util.getVirtualFile
import com.ruin.lsp.values.Range
import com.ruin.lsp.values.SymbolInformation
import com.ruin.lsp.values.TextDocumentIdentifier

abstract class DocumentSymbolCommandTestBase : BaseTestCase() {
    override val projectName = JAVA_PROJECT

    protected fun getSymbols(filePath: String): List<SymbolInformation> {
        val file = getVirtualFile(project, filePath)
        val command = DocumentSymbolCommand(TextDocumentIdentifier(file.url))
        val result = execute(command, file.url)
        command.dispose()
        return result
    }

    protected fun List<SymbolInformation>.assertHasSymbol(
        expectedName: String,
        expectedParent: String?,
        expectedKind: Int,
        expectedRange: Range): List<SymbolInformation> = apply {
        val symbol = asSequence().firstOrNull {
            it.name == expectedName && it.containerName == expectedParent
        }
        assertNotNull("$expectedName($expectedParent) is missing from $this", symbol)
        assertEquals("$expectedName($expectedParent): unexpected SymbolKind", expectedKind, symbol!!.kind)
        assertEquals("$expectedName($expectedParent): unexpected Range", expectedRange, symbol.location.range)
    }
}
