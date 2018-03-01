package com.ruin.intel.model

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Document
import com.intellij.psi.PsiDocumentManager
import com.intellij.util.diff.Diff
import com.ruin.intel.Util.*
import com.ruin.intel.values.*
import groovy.util.GroovyTestCase.assertEquals
import com.intellij.openapi.command.UndoConfirmationPolicy
import com.intellij.openapi.util.TextRange


val LOG = Logger.getInstance(WorkspaceManager::class.java)

/**
 * Manages files opened by LSP clients.
 *
 * The idea is that we want to deal with PSI files as little as possible due to threading constraints and the fact that
 * they constantly go invalid. Instead, this class keeps a separate ground truth, and when document changes come in, it
 * is possible to reload the corresponding PSI file with the changed contents.
 */
class WorkspaceManager {
    val managedFiles: HashMap<DocumentUri, ManagedTextDocument> = HashMap()

    fun onTextDocumentOpened(textDocument: TextDocumentItem) {
        if(managedFiles.containsKey(textDocument.uri)) {
            LOG.warn("URI ${textDocument.uri} was opened again without being closed")
            return
        }

        LOG.debug("Handling textDocument/didOpen for ${textDocument.uri}")

        ApplicationManager.getApplication().invokeAndWait(asWriteAction(Runnable {
            reloadDocumentAtUri(textDocument.uri)
        }))

        managedFiles[textDocument.uri] =
            ManagedTextDocument(
                VersionedTextDocumentIdentifier(textDocument.uri, textDocument.version), textDocument.text
            )
    }

    fun onTextDocumentClosed(textDocument: VersionedTextDocumentIdentifier) {
        if(!managedFiles.containsKey(textDocument.uri)) {
            LOG.warn("Attempted to close text document at ${textDocument.uri} without opening it")
            return
        }

        LOG.debug("Handling textDocument/didClose for ${textDocument.uri}")

        val managedTextDoc = managedFiles[textDocument.uri]!!

        if(managedTextDoc.identifier.version != textDocument.version) {
            LOG.warn("Document version differed on close - " +
                "ours: ${managedTextDoc.identifier.version}, theirs: ${textDocument.version}")
        }

        managedFiles.remove(textDocument.uri)
    }

    fun onTextDocumentChanged(textDocument: VersionedTextDocumentIdentifier,
                              contentChanges: List<TextDocumentContentChangeEvent>) {
        if(!managedFiles.containsKey(textDocument.uri)) {
            LOG.warn("Tried handling didChange for ${textDocument.uri}, but it wasn't open")
            return
        }

        LOG.debug("Handling textDocument/didChange")
        LOG.debug("textDocument: $textDocument")
        LOG.debug("contentChanges: $contentChanges")
        LOG.debug("Version before: ${managedFiles[textDocument.uri]!!.identifier.version}")

        val managedTextDoc = managedFiles[textDocument.uri]!!

        // Version number of our document should be (theirs - 1)
        if(managedTextDoc.identifier.version != (textDocument.version!! - 1)) {
            LOG.warn("Version mismatch on document change - " +
                "ours: ${managedTextDoc.identifier.version}, theirs: ${textDocument.version}")
            return
        }

        val pair = resolvePsiFromUri(textDocument.uri)

        if(pair == null) {
            LOG.warn("Couldn't resolve Psi file at ${textDocument.uri}")
            return
        }

        val (project, _) = pair

        ApplicationManager.getApplication().invokeAndWait {
            CommandProcessor.getInstance().executeCommand(project, asWriteAction( Runnable {
                val doc = getDocument(textDocument.uri)

                if (doc != null) {
                    assertEquals("Document and ground truth don't match!" +
                    "\n\n${doc.text}\n\n=====\n\n${managedTextDoc.contents}",
                        doc.text, managedTextDoc.contents)
                    LOG.debug("Doc before:\n\n${doc.text}\n\n")

                    assert(doc.isWritable, { "Document at ${textDocument.uri} wasn't writable!" })
                    applyChanges(doc, contentChanges)

                    LOG.debug("Doc after:\n\n${doc.text}\n\n")

                    // Commit changes to the PSI tree, but not to disk
                    PsiDocumentManager.getInstance(project).commitDocument(doc)

                    // Update the ground truth
                    val newDoc =
                        ManagedTextDocument(
                            VersionedTextDocumentIdentifier(textDocument.uri, textDocument.version), doc.text
                        )

                    managedFiles[textDocument.uri] = newDoc
                } else {
                    LOG.warn("Attempted to get Document for updating but it was null: ${textDocument.uri}")
                }
            }), "HandleTextDocumentDidChange", "", UndoConfirmationPolicy.REQUEST_CONFIRMATION)
        }


        LOG.debug("Version after: ${managedFiles[textDocument.uri]!!.identifier.version}")
    }

    fun onTextDocumentSaved(textDocument: VersionedTextDocumentIdentifier, text: String?) {
        if (!managedFiles.containsKey(textDocument.uri)) {
            LOG.warn("Tried handling didSave for ${textDocument.uri}, but it wasn't open")
            return
        }

        LOG.debug("Handling textDocument/didSave for ${textDocument.uri}")

        val managedTextDoc = managedFiles[textDocument.uri]!!

        assertEquals("Document version differed on save - " +
            "ours: ${managedTextDoc.identifier.version}, theirs: ${textDocument.version}",
            textDocument.version, managedTextDoc.identifier.version)

        if (text != null) {
            assert(managedTextDoc.contents == text, {
                val change = Diff.buildChanges(managedTextDoc.contents, text)
                "Ground truth differed upon save!\n\n$change"
            })
        }
    }
}

data class ManagedTextDocument(var identifier: VersionedTextDocumentIdentifier, var contents: String)

fun rangeToTextRange(doc: Document, range: Range): TextRange {
    val startLineOffset = doc.getLineStartOffset(range.start.line)
    val startOffset = startLineOffset + range.start.character
    val endLineOffset = doc.getLineStartOffset(range.end.line)
    val endOffset = endLineOffset + range.end.character

    return TextRange(startOffset, endOffset)
}

fun applyChanges(doc: Document, contentChanges: List<TextDocumentContentChangeEvent>) =
    contentChanges.forEach { applyChange(doc, it) }

fun applyChange(doc: Document, change: TextEdit) {
    LOG.debug("Applying change: $change")
    val textRange = rangeToTextRange(doc, change.range)
    doc.replaceString(textRange.startOffset, textRange.endOffset, change.newText)
}
fun applyChange(doc: Document, change: TextDocumentContentChangeEvent) {
    LOG.debug("Applying change: $change")
    if(change.range == null) {
        // Change is the full text of the document
        doc.setText(change.text)
    } else {
        val textRange = rangeToTextRange(doc, change.range)
        doc.replaceString(textRange.startOffset, textRange.endOffset, change.text)
    }
}
