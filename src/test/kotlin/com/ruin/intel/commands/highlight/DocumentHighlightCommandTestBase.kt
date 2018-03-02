package com.ruin.intel.commands.highlight

import com.ruin.intel.BaseTestCase
import com.ruin.intel.JAVA_PROJECT
import com.ruin.intel.Util.getVirtualFile
import com.ruin.intel.commands.completion.CompletionCommand
import com.ruin.intel.model.execute
import com.ruin.intel.values.DocumentHighlight
import com.ruin.intel.values.Position
import com.ruin.intel.values.TextDocumentIdentifier

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
