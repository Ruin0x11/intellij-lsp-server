package com.ruin.lsp.commands.document.highlight

import com.ruin.lsp.BaseTestCase
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.model.invokeCommandAndWait
import com.ruin.lsp.util.getURIForFile
import com.ruin.lsp.util.getVirtualFile
import com.ruin.lsp.util.resolvePsiFromUri
import org.eclipse.lsp4j.DocumentHighlight
import org.eclipse.lsp4j.Position
import java.io.File

abstract class DocumentHighlightCommandTestBase : BaseTestCase() {
    protected fun checkHighlightsFound(filePath: String, at: Position, expected: List<DocumentHighlight>) {
        val file = resolvePsiFromUri(project, getURIForFile(File(filePath)))!!
        val command = DocumentHighlightCommand(at)
        val result = invokeCommandAndWait(command, project, file)
        assertSameElements(result, expected)
    }
}
