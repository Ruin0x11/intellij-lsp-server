package com.ruin.lsp.commands.symbol

import com.ruin.lsp.BaseTestCase
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.model.invokeCommandAndWait
import com.ruin.lsp.util.getVirtualFile
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.SymbolInformation
import org.eclipse.lsp4j.SymbolKind
import org.eclipse.lsp4j.TextDocumentIdentifier

abstract class DocumentSymbolCommandTestBase : BaseTestCase() {
    override val projectName = JAVA_PROJECT

    protected fun getSymbols(filePath: String): List<SymbolInformation> {
        val file = getVirtualFile(project, filePath)
        val command = DocumentSymbolCommand(TextDocumentIdentifier(file.url))
        val result = invokeCommandAndWait(command, file.url)
        command.dispose()
        return result
    }

    protected fun List<SymbolInformation>.assertHasSymbol(
        expectedName: String,
        expectedParent: String?,
        expectedKind: SymbolKind,
        expectedRange: Range): List<SymbolInformation> = apply {
        val symbol = asSequence().firstOrNull {
            it.name == expectedName && it.containerName == expectedParent
        }
        assertNotNull("$expectedName($expectedParent) is missing from $this", symbol)
        assertEquals("$expectedName($expectedParent): unexpected SymbolKind", expectedKind, symbol!!.kind)
        assertEquals("$expectedName($expectedParent): unexpected Range", expectedRange, symbol.location.range)
    }
}
