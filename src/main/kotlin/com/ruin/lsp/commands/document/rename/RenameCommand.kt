package com.ruin.lsp.commands.document.rename

import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.util.Ref
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.model.workspace
import com.ruin.lsp.util.*
import com.ruin.lsp.values.DocumentUri
import org.eclipse.lsp4j.*

val LOG = Logger.getInstance(RenameCommand::class.java)

// TODO: Doesn't work with this.thing = thing;
class RenameCommand(val position: Position, val newName: String) : DocumentCommand<WorkspaceEdit> {
    override fun execute(ctx: ExecutionContext): WorkspaceEdit {
        val ref: Ref<List<RenameRange>> = Ref()
        withEditor(this, ctx.file, position) { editor ->
            val element = findTargetElement(editor) ?: return@withEditor
            // TODO: Make "search in text/comments" configurable?
            val processor = RangeGatheringRenameProcessor(ctx.project, element, newName)
            processor.doRun()
            ref.set(processor.refs.toList())
        }

        val rawResults = ref.get()

        if (rawResults.isEmpty()) {
            return WorkspaceEdit()
        }

        val edits = rawResults.mapNotNull { range ->
            range.toEditAndFile()
        }
        val results = groupEdits(edits)

        return WorkspaceEdit(results)
    }
}

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
        val identifier = VersionedTextDocumentIdentifier().apply {
            this.uri = uri
            // If the file was opened on the server, we should be able to send 'null' indicating the file
            // on disk is the ground truth. However, lsp-mode always expects the version to be a number.
            this.version = workspace.getCurrentDocumentVersion(uri) ?: 0
        }
        // For some reason an edit is counted twice when renaming a class.
        TextDocumentEdit(identifier, edits.distinct())
    }
}
