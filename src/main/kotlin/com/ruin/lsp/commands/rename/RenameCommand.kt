package com.ruin.lsp.commands.rename

import com.github.kittinunf.result.Result
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Document
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Ref
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.ruin.lsp.commands.Command
import com.ruin.lsp.commands.errorResult
import com.ruin.lsp.commands.find.*
import com.ruin.lsp.model.workspace
import com.ruin.lsp.util.findTargetElement
import com.ruin.lsp.util.getDocument
import com.ruin.lsp.util.getURIForFile
import com.ruin.lsp.util.withEditor
import com.ruin.lsp.values.*

val LOG = Logger.getInstance(RenameCommand::class.java)

// TODO: Doesn't work with this.thing = thing;
class RenameCommand(val textDocumentIdentifier: TextDocumentIdentifier,
                    val position: Position,
                    val newName: String) : Command<WorkspaceEdit> {
    override fun execute(project: Project, file: PsiFile): Result<WorkspaceEdit, Exception> {
        val ref: Ref<List<RenameRange>> = Ref()
        withEditor(this, file, position) { editor ->
            val element = findTargetElement(editor) ?: return@withEditor
            // TODO: Make "search in text/comments" configurable?
            val processor = RangeGatheringRenameProcessor(project, element, newName)
            processor.doRun()
            ref.set(processor.refs.toList())
        }

        val rawResults = ref.get()

        if (rawResults.isEmpty()) {
            return emptyResult()
        }

        val edits = rawResults.mapNotNull { range ->
            range.toEditAndFile()
        }
        val results = groupEdits(edits)

        return Result.of(WorkspaceEdit(documentChanges = results))
    }
}

private fun emptyResult() = Result.of(WorkspaceEdit(documentChanges = listOf()))

private fun RenameRange.toEditAndFile(): Pair<DocumentUri, TextEdit>? {
    val doc = getDocument(file)!!
    val range = textRange.toRange(doc)
    val uri = getURIForFile(file)
    val edit = TextEdit(range, newName)

    return Pair(uri, edit)
}

private fun groupEdits(edits: List<Pair<DocumentUri, TextEdit>>): List<TextDocumentEdit> {
    val workspace = workspace()
    val documentChanges: MutableMap<DocumentUri, MutableList<TextEdit>> = mutableMapOf()

    edits.forEach { (uri, edit) ->
        val textDocumentEdits = documentChanges.getOrPut(uri) { mutableListOf() }

        textDocumentEdits.add(edit)
    }

    return documentChanges.map { (uri, edits) ->
        val identifier = VersionedTextDocumentIdentifier(uri,
            // If the file was opened on the server, we should be able to send 'null' indicating the file
            // on disk is the ground truth. However, lsp-mode always expects the version to be a number.
            workspace.getCurrentDocumentVersion(uri) ?: 0)
        // For some reason an edit is counted twice when renaming a class.
        TextDocumentEdit(identifier, edits.distinct())
    }
}
