package com.ruin.lsp

import com.ruin.lsp.model.WorkspaceManager
import com.ruin.lsp.values.Position
import com.ruin.lsp.values.Range
import com.ruin.lsp.values.TextDocumentContentChangeEvent
import org.intellivim.FileEditingTestCase

class WorkspaceManagerTest : FileEditingTestCase() {
    override val filePath: String
        get() = DUMMY_FILE_PATH

    override val projectName: String
        get() = JAVA_PROJECT

    fun `test increments version number on write`() {
        val manager = WorkspaceManager()

        manager.onTextDocumentOpened(textDocumentItem(0))

        val changes =
            listOf(TextDocumentContentChangeEvent(null, null, "dood"))
        manager.onTextDocumentChanged(getVersionedTextDocumentIdentifier(1), changes)

        assertEquals(1, manager.managedFiles[file.url]!!.identifier.version)
    }

    fun `test full text update`() {
        val manager = WorkspaceManager()

        manager.onTextDocumentOpened(textDocumentItem(0))

        val changes =
            listOf(TextDocumentContentChangeEvent(null, null, "dood"))
        manager.onTextDocumentChanged(getVersionedTextDocumentIdentifier(1), changes)

        assertEquals("dood", manager.managedFiles[file.url]!!.contents)
        assertPsiContentsChanged()
    }

    fun `test partial text update`() {
        val manager = WorkspaceManager()

        manager.onTextDocumentOpened(textDocumentItem(0))

        val range = Range(Position(11, 28), Position(11, 30))
        val changes =
            listOf(TextDocumentContentChangeEvent(range, 2, "dood"))
        manager.onTextDocumentChanged(getVersionedTextDocumentIdentifier(1), changes)

        assert(manager.managedFiles[file.url]!!.contents.contains("System.out.println(\"dood\");"))
        assertPsiContentsChanged()
    }
}
