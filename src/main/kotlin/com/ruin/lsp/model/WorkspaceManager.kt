package com.ruin.lsp.model

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.command.UndoConfirmationPolicy
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Document
import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.openapi.util.Computable
import com.intellij.openapi.util.Ref
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiDocumentManager
import com.intellij.util.diff.Diff
import com.ruin.lsp.util.*
import com.ruin.lsp.values.DocumentUri
import org.eclipse.lsp4j.*


private val LOG = Logger.getInstance(WorkspaceManager::class.java)

/**
 * Manages files opened by LSP clients.
 *
 * The idea is that we want to deal with PSI files as little as possible due to threading constraints and the fact that
 * they constantly go invalid. Instead, this class keeps a separate ground truth, and when document changes come in, it
 * is possible to reload the corresponding PSI file with the changed contents.
 */
class WorkspaceManager {
    val managedTextDocuments: HashMap<DocumentUri, ManagedTextDocument> = HashMap()

    @Synchronized
    fun onTextDocumentOpened(params: DidOpenTextDocumentParams,
                             client: MyLanguageClient? = null,
                             server: MyLanguageServer? = null) {
        val textDocument = params.textDocument

        if(managedTextDocuments.containsKey(textDocument.uri)) {
            LOG.warn("URI was opened again without being closed, resetting: ${textDocument.uri}")
            managedTextDocuments.remove(textDocument.uri)
        }

        LOG.debug("Handling textDocument/didOpen for ${textDocument.uri}")

        val success = invokeAndWaitIfNeeded(asWriteAction(Computable<Boolean> {
            val doc = getDocument(textDocument.uri) ?: return@Computable false
            val project = resolveProjectFromUri(textDocument.uri)?.first ?: return@Computable false
            reloadDocument(doc, project)
            if (client != null) {
                if (server != null) {
                    registerIndexNotifier(project, client, server)
                }
                val projectSdk = ProjectRootManager.getInstance(project).projectSdk
                if (projectSdk == null) {
                    client.showMessage(MessageParams(MessageType.Warning,
                        "Project SDK is not defined. Use idea/openProjectStructure to set it up."))
                }
            }
            true
        }))

        if(!success) {
            LOG.warn("Couldn't open Document for ${textDocument.uri}!")
            return
        }

        managedTextDocuments[textDocument.uri] =
            ManagedTextDocument(
                VersionedTextDocumentIdentifier().apply {
                    uri = textDocument.uri
                    version = textDocument.version
                },
                textDocument.text.replace("\r\n", "\n")
            )
    }

    @Synchronized
    fun onTextDocumentClosed(params: DidCloseTextDocumentParams) {
        val textDocument = params.textDocument

        if(!managedTextDocuments.containsKey(textDocument.uri)) {
            LOG.warn("Attempted to close document without opening it at: ${textDocument.uri}")
            return
        }

        LOG.debug("Handling textDocument/didClose for ${textDocument.uri}")

        managedTextDocuments.remove(textDocument.uri)
    }

    @Synchronized
    fun onTextDocumentChanged(params: DidChangeTextDocumentParams) {
        val textDocument = params.textDocument
        val contentChanges = params.contentChanges

        if(!managedTextDocuments.containsKey(textDocument.uri)) {
            LOG.warn("Tried handling didChange for ${textDocument.uri}, but it wasn't open")
            return
        }

        LOG.debug("Handling textDocument/didChange")
        LOG.debug("textDocument: $textDocument")
        LOG.debug("contentChanges: $contentChanges")
        LOG.debug("Version before: ${managedTextDocuments[textDocument.uri]!!.identifier.version}")

        runDocumentUpdate(textDocument) { doc ->
            applyContentChangeEventChanges(doc, sortContentChangeEventChanges(contentChanges))
        }

        LOG.debug("Version after: ${managedTextDocuments[textDocument.uri]!!.identifier.version}")
    }

    @Synchronized
    fun onTextDocumentSaved(params: DidSaveTextDocumentParams) {
        val textDocument = params.textDocument
        val text = params.text

        if (!managedTextDocuments.containsKey(textDocument.uri)) {
            LOG.warn("Tried handling didSave for ${textDocument.uri}, but it wasn't open")
            return
        }

        LOG.debug("Handling textDocument/didSave for ${textDocument.uri}")

        val managedTextDoc = managedTextDocuments[textDocument.uri]!!

        ApplicationManager.getApplication().invokeAndWait(asWriteAction( Runnable {
            val (project, file) = resolvePsiFromUri(textDocument.uri) ?: return@Runnable
            val document = getDocument(file) ?: return@Runnable
            reloadDocument(document, project)
            LOG.debug("Reloaded document at ${textDocument.uri}")
        }))

        if (text != null) {
            assert(managedTextDoc.contents == text, {
                val change = Diff.buildChanges(managedTextDoc.contents, text)
                LOG.debug("Difference: $change")
                "Ground truth differed upon save!"
            })
        }
    }

    @Synchronized
    fun onWorkspaceApplyEdit(label: String?, edit: WorkspaceEdit): ApplyWorkspaceEditResponse {
        LOG.debug("Handling workspace/applyEdit")
        LOG.debug("edit: $edit")

        var applied = false
        edit.documentChanges?.forEach { textDocumentEdit ->
            val result = applyTextDocumentEdit(textDocumentEdit)
            applied = applied || result
        }

        return ApplyWorkspaceEditResponse(applied)
    }

    private fun applyTextDocumentEdit(edit: TextDocumentEdit): Boolean {
        if (!managedTextDocuments.containsKey(edit.textDocument.uri)) {
            val success = openFileFromTextDocumentEdit(edit)
            if (!success) {
                LOG.warn("Tried applying TextDocumentEdit for ${edit.textDocument.uri}, but it couldn't be opened")
                return false
            }
        } else {
            LOG.debug("URI ${edit.textDocument.uri} is open already.")
        }

        LOG.debug("Applying TextDocumentEdit as part of workspace/applyEdit")
        LOG.debug("textDocument: ${edit.textDocument}")
        LOG.debug("edits: ${edit.edits}")
        LOG.debug("Version before: ${managedTextDocuments[edit.textDocument.uri]!!.identifier.version}")

        val result = runDocumentUpdate(edit.textDocument) { doc ->
            applyTextEditChanges(doc, sortTextEditChanges(edit.edits))
        }

        LOG.debug("Version after: ${managedTextDocuments[edit.textDocument.uri]!!.identifier.version}")

        return result
    }

    /**
     * @return true if the open succeeds
     */
    private fun openFileFromTextDocumentEdit(edit: TextDocumentEdit): Boolean {
        LOG.debug("Opening document from a WorkspaceEdit for ${edit.textDocument.uri}")

        val text = invokeAndWaitIfNeeded(asWriteAction(Computable<String?> {
            val doc = getDocument(edit.textDocument.uri) ?: return@Computable null
            val project = resolveProjectFromUri(edit.textDocument.uri)?.first ?: return@Computable null
            reloadDocument(doc, project)
            doc.text
        }))

        if(text == null) {
            LOG.warn("Couldn't open Document for ${edit.textDocument.uri}!")
            return false
        }

        managedTextDocuments[edit.textDocument.uri] =
            ManagedTextDocument(
                VersionedTextDocumentIdentifier().apply {
                    uri = edit.textDocument.uri
                    version = 0 // TODO: should this be null?
                },
                normalizeText(text)
            )

        return true
    }

    /**
     * @return true if the update succeeds
     */
    private fun runDocumentUpdate(textDocument: VersionedTextDocumentIdentifier, callback: (Document) -> Unit): Boolean {
        val managedTextDoc = managedTextDocuments[textDocument.uri]!!

        // Version number of our document should be (theirs - 1)
        if (managedTextDoc.identifier.version != (textDocument.version - 1)) {
            LOG.warn("Version mismatch on document change - " +
                "ours: ${managedTextDoc.identifier.version}, theirs: ${textDocument.version}")
            return false
        }

        val pair = resolvePsiFromUri(textDocument.uri)

        if(pair == null) {
            LOG.warn("Couldn't resolve Psi file at ${textDocument.uri}")
            return false
        }

        val (project, _) = pair
        val ref: Ref<Boolean> = Ref(false)
        ApplicationManager.getApplication().invokeAndWait {
            CommandProcessor.getInstance().executeCommand(project, asWriteAction( Runnable {
                val doc = getDocument(textDocument.uri)

                if (doc != null) {
                    if(managedTextDoc.contents != doc.text) {
                        val change = Diff.buildChanges(managedTextDoc.contents, doc.text)
                        LOG.debug("Difference: $change")
                        LOG.warn("Ground truth differed upon change!")
                        return@Runnable
                    }
                    LOG.debug("Doc before:\n\n${doc.text}\n\n")

                    if(!doc.isWritable) {
                        LOG.warn("Document at ${textDocument.uri} wasn't writable!")
                        return@Runnable
                    }

                    try {
                        callback(doc)
                    } catch (e: Exception) {
                        LOG.error("Error on documentChange: " + e.stackTrace)
                    }

                    LOG.debug("Doc after:\n\n${doc.text}\n\n")

                    // Commit changes to the PSI tree, but not to disk
                    PsiDocumentManager.getInstance(project).commitDocument(doc)

                    // Update the ground truth
                    val newVersion = textDocument.version
                    val newDoc =
                        ManagedTextDocument(
                            VersionedTextDocumentIdentifier().apply {
                                uri = textDocument.uri
                                version = newVersion
                            },
                            normalizeText(doc.text)
                        )

                    managedTextDocuments[textDocument.uri] = newDoc
                    ref.set(true)
                } else {
                    LOG.warn("Attempted to get Document for updating but it was null: ${textDocument.uri}")
                }
            }), "runDocumentUpdate", "", UndoConfirmationPolicy.REQUEST_CONFIRMATION)
        }
        return ref.get()
    }

    @Synchronized
    fun onShutdown() {
        managedTextDocuments.values.forEach { onTextDocumentClosed(DidCloseTextDocumentParams(it.identifier)) }
    }
}

data class ManagedTextDocument(var identifier: VersionedTextDocumentIdentifier, var contents: String)

private fun normalizeText(text: String) = text.replace("\r\n", "\n")


/**
 * Sorts text edits from furthest in the file to nearest to the top of the file.
 *
 * Prevents issues with the actual text edit range changing when applying multiple edits in sequence.
 */
fun sortTextEditChanges(edits: List<TextEdit>?): List<TextEdit>? =
    edits?.sortedWith(compareBy({ it.range.start.line }, { it.range.start.character }))?.reversed()

fun sortContentChangeEventChanges(edits: List<TextDocumentContentChangeEvent>?): List<TextDocumentContentChangeEvent>? =
    edits?.sortedWith(compareBy({ it.range.start.line }, { it.range.start.character }))?.reversed()


fun applyTextEditChanges(doc: Document, contentChanges: List<TextEdit>?) =
    contentChanges?.forEach { applyChange(doc, it) }

fun applyContentChangeEventChanges(doc: Document, contentChanges: List<TextDocumentContentChangeEvent>?) =
    contentChanges?.forEach { applyChange(doc, it) }

private fun applyChange(doc: Document, change: TextEdit) {
    LOG.debug("Applying change: $change")
    val textRange = change.range.toTextRange(doc)
    doc.replaceString(textRange.startOffset, textRange.endOffset, change.newText)
}
private fun applyChange(doc: Document, change: TextDocumentContentChangeEvent) {
    LOG.debug("Applying change: $change")
    if(change.range == null) {
        // Change is the full insertText of the document
        doc.setText(change.text)
    } else {
        val textRange = change.range.toTextRange(doc)
        doc.replaceString(textRange.startOffset, textRange.endOffset, change.text)
    }
}
