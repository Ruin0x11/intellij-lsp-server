package com.ruin.lsp.commands.highlight

import com.ruin.lsp.BaseTestCase
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.util.getVirtualFile
import com.ruin.lsp.model.execute
import com.ruin.lsp.values.DocumentHighlight
import com.ruin.lsp.values.Position
import com.ruin.lsp.values.TextDocumentIdentifier

abstract class DocumentHighlightCommandTestBase : BaseTestCase() {
    override val projectName: String
        get() = JAVA_PROJECT

    protected fun checkHighlightsFound(filePath: String, at: Position, expected: List<DocumentHighlight>) {
        val file = getVirtualFile(project, filePath)
        val command = DocumentHighlightCommand(TextDocumentIdentifier(file.url), at)
        val result = execute(command, file.url)
        assertSameElements(result, expected)
        command.dispose()
    }
}
