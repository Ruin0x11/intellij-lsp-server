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

class RenameCommand(val textDocumentIdentifier: VersionedTextDocumentIdentifier,
                    val position: Position,
                    val newName: String) : Command<WorkspaceEdit> {
    override fun execute(project: Project, file: PsiFile): Result<WorkspaceEdit, Exception> {
        val doc = getDocument(file) ?: return errorResult("Document not found.")

        val ref: Ref<List<RenameRange>> = Ref()
        withEditor(this, file, position) { editor ->
            val element = findTargetElement(editor) ?: return@withEditor
            // TODO: Make "search in text/comments" configurable?
            val processor = RangeGatheringRenameProcessor(project, element, newName, editor)
            processor.doRun()
            ref.set(processor.refs.toList())
        }

        val rawResults = ref.get()

        if (rawResults.isEmpty()) {
            return emptyResult()
        }

        val edits = rawResults.mapNotNull { range ->
            range.toEditAndFile(doc)
        }
        val results = groupEdits(edits)

        return Result.of(WorkspaceEdit(documentChanges = results))
    }
}

private fun emptyResult() = Result.of(WorkspaceEdit(documentChanges = listOf()))

private fun RenameRange.toEditAndFile(doc: Document): Pair<DocumentUri, TextEdit>? {
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
        val identifier = VersionedTextDocumentIdentifier(uri, workspace.getCurrentDocumentVersion(uri))
        TextDocumentEdit(identifier, edits)
    }
}
