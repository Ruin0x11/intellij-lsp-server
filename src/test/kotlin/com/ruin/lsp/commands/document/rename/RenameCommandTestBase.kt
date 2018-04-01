package com.ruin.lsp.commands.document.rename

import com.ruin.lsp.BaseTestCase
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.model.execute
import com.ruin.lsp.model.workspace
import com.ruin.lsp.util.getPsiFile
import com.ruin.lsp.util.getURIForFile
import com.ruin.lsp.util.normalizeUri
import java.io.FileNotFoundException

abstract class RenameCommandTestBase : BaseTestCase() {
    override val projectName = JAVA_PROJECT

    protected fun checkRenameHas(filePath: String, at: Position, newName: String, expected: List<Pair<String, List<TextEdit>>>) {
        val file = getPsiFile(project, filePath) ?: throw FileNotFoundException(filePath)
        val uri = getURIForFile(file)
        workspace().onTextDocumentOpened(TextDocumentItem(uri, "java", 0, file.text))
        val command = RenameCommand(TextDocumentIdentifier(getURIForFile(file)), at, newName)
        val result = execute(command, uri)
        assertEquals(result.documentChanges!!.size, expected.size)

        result.documentChanges!!.forEach { edit ->
            val file = expected.first { (path, _) ->
                edit.textDocument.uri.contains(normalizeUri(path))
            }

            assertSameElements(edit.edits, file.second)
        }
    }
}
