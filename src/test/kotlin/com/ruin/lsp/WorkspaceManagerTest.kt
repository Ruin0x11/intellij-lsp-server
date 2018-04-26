package com.ruin.lsp

import com.ruin.lsp.model.WorkspaceManager
import org.eclipse.lsp4j.*
import org.intellivim.FileEditingTestCase

class WorkspaceManagerTest : FileEditingTestCase() {
    override val filePath: String
        get() = DUMMY_FILE_PATH

    override val projectName: String
        get() = JAVA_PROJECT

    fun `test increments version number on write`() {
        val manager = WorkspaceManager()

        manager.onTextDocumentOpened(DidOpenTextDocumentParams(makeTextDocumentItem(0)), project)

        val changes =
            listOf(TextDocumentContentChangeEvent(null, null, "dood"))
        manager.onTextDocumentChanged(
            DidChangeTextDocumentParams(makeVersionedTextDocumentIdentifier(1), changes), project
        )

        assertEquals(1, manager.managedTextDocuments[file.url]!!.identifier.version)
    }

    fun `test full text update`() {
        val manager = WorkspaceManager()

        manager.onTextDocumentOpened(DidOpenTextDocumentParams(makeTextDocumentItem(0)), project)

        val changes =
            listOf(TextDocumentContentChangeEvent(null, null, "dood"))
        manager.onTextDocumentChanged(
            DidChangeTextDocumentParams(makeVersionedTextDocumentIdentifier(1), changes), project
        )

        assertEquals("dood", manager.managedTextDocuments[file.url]!!.contents)
        assertPsiContentsChanged()
    }

    fun `test partial text update`() {
        val manager = WorkspaceManager()

        manager.onTextDocumentOpened(DidOpenTextDocumentParams(makeTextDocumentItem(0)), project)

        val range = range(11, 28, 11, 30)
        val changes =
            listOf(TextDocumentContentChangeEvent(range, 2, "dood"))
        manager.onTextDocumentChanged(
            DidChangeTextDocumentParams(makeVersionedTextDocumentIdentifier(1), changes), project
        )

        assert(manager.managedTextDocuments[file.url]!!.contents.contains("System.out.println(\"dood\");"))
        assertPsiContentsChanged()
    }

    fun `test workspace edit`() {
        val manager = WorkspaceManager()

        manager.onTextDocumentOpened(
            DidOpenTextDocumentParams(makeTextDocumentItem(0)), project
        )

        // not currently opened
        val otherId = makeVersionedTextDocumentIdentifier(SUBCLASS_FILE_PATH, 0)

        val firstRange = Range(Position(11, 28), Position(11, 30))
        val firstChanges = listOf(TextEdit(firstRange, "dood"))
        val firstEdit = TextDocumentEdit(makeVersionedTextDocumentIdentifier(1), firstChanges)

        val secondEdit = TextDocumentEdit(otherId, listOf())

        val changes = listOf(firstEdit, secondEdit)

        val workspaceEdit = WorkspaceEdit(changes)

        val result = manager.onWorkspaceApplyEdit(null, workspaceEdit, project)
        assert(result.applied, { "Workspace edit wasn't successful" })

        // now the file should have opened
        assert(manager.managedTextDocuments.containsKey(otherId.uri), { "File ${otherId.uri} wasn't opened." })

        assert(manager.managedTextDocuments[file.url]!!.contents.contains("System.out.println(\"dood\");"))
        assertPsiContentsChanged()
    }

    fun `test shutdown`() {
        val manager = WorkspaceManager()

        manager.onTextDocumentOpened(DidOpenTextDocumentParams(makeTextDocumentItem(0)), project)

        val changes =
            listOf(TextDocumentContentChangeEvent(null, null, "dood"))
        manager.onTextDocumentChanged(
            DidChangeTextDocumentParams(makeVersionedTextDocumentIdentifier(1), changes), project
        )

        manager.onShutdown()

        manager.onTextDocumentOpened(
            DidOpenTextDocumentParams(makeTextDocumentItem(0)), project
        )
        assert("dood" != manager.managedTextDocuments[file.url]!!.contents)
    }

    fun `test updates on open with client unsaved changes`() {
        val manager = WorkspaceManager()

        manager.onTextDocumentOpened(DidOpenTextDocumentParams(makeTextDocumentItem(0).apply { text = "dood" }), project)

        assertPsiContentsChanged()
        assertVirtualFileContentsUnchanged()
        assert("dood" == manager.managedTextDocuments[file.url]!!.contents)
    }
}
