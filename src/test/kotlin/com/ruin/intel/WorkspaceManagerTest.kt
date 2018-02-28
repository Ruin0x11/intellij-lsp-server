package com.ruin.intel

import com.ruin.intel.model.WorkspaceManager
import com.ruin.intel.values.TextDocumentContentChangeEvent
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
        manager.onTextDocumentChanged(versionedTextDocumentIdentifier(1), changes)

        assertEquals(1, manager.managedFiles[file.url]!!.identifier.version)
    }

    fun `test full text update`() {
        val manager = WorkspaceManager()

        manager.onTextDocumentOpened(textDocumentItem(0))

        val changes =
            listOf(TextDocumentContentChangeEvent(null, null, "dood"))
        manager.onTextDocumentChanged(versionedTextDocumentIdentifier(1), changes)

        assertEquals("dood", manager.managedFiles[file.url]!!.contents)
        assertVirtualFileNowContains("dood")
        assertPsiContentsChanged()
    }

    fun `test partial text update`() {
        assert(false)
    }

    fun `test range to start and end offset`() {
        assert(false)
    }
}
