package com.ruin.lsp.commands.highlight

import com.ruin.lsp.BaseTestCase
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.model.invokeCommandAndWait
import com.ruin.lsp.util.getVirtualFile
import org.eclipse.lsp4j.DocumentHighlight
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.TextDocumentIdentifier

abstract class DocumentHighlightCommandTestBase : BaseTestCase() {
    override val projectName: String
        get() = JAVA_PROJECT

    protected fun checkHighlightsFound(filePath: String, at: Position, expected: List<DocumentHighlight>) {
        val file = getVirtualFile(project, filePath)
        val command = DocumentHighlightCommand(at)
        val result = invokeCommandAndWait(command, file.url)
        assertSameElements(result, expected)
    }
}
