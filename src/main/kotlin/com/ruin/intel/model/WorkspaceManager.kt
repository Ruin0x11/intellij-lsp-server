package com.ruin.intel.model

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Document
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiManager
import com.intellij.util.FileContentUtil
import com.intellij.util.diff.Diff
import com.ruin.intel.Util.*
import com.ruin.intel.values.*
import groovy.util.GroovyTestCase.assertEquals
import com.intellij.openapi.command.UndoConfirmationPolicy
import com.sun.javafx.scene.CameraHelper.project
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.editor.Editor



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

    fun onTextDocumentClosed(textDocument: TextDocumentIdentifier) {
        if(!managedFiles.containsKey(textDocument.uri)) {
            LOG.warn("Attempted to close text document at ${textDocument.uri} without opening it")
            return
        }

        LOG.debug("Handling textDocument/didClose for ${textDocument.uri}")

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
        LOG.debug("Truth before:\n\n${managedFiles[textDocument.uri]!!.contents}\n\n")

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

        val (project, file) = pair

        ApplicationManager.getApplication().invokeAndWait {
            CommandProcessor.getInstance().executeCommand(project, asWriteAction( Runnable {
                val doc = getDocument(textDocument.uri)

                if (doc != null) {
                    LOG.debug("Doc before:\n\n${doc.text}\n\n")

                    assert(doc.isWritable, { "Document at ${textDocument.uri} wasn't writable!" })
                    applyChanges(doc, contentChanges)

                    LOG.debug("Doc after:\n\n${doc.text}\n\n")

                    // Commit changes to the PSI tree, but not to disk
                    PsiDocumentManager.getInstance(project).commitDocument(doc)

                    // Force a reparse of the file.
                    FileContentUtil.reparseFiles(file.virtualFile)

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
        LOG.debug("Truth after:\n\n${managedFiles[textDocument.uri]!!.contents}\n\n")
    }

    fun onTextDocumentSaved(textDocument: TextDocumentIdentifier, text: String?) {
        if (!managedFiles.containsKey(textDocument.uri)) {
            LOG.warn("Tried handling didSave for ${textDocument.uri}, but it wasn't open")
            return
        }

        LOG.debug("Handling textDocument/didSave for ${textDocument.uri}")

        val managedTextDoc = managedFiles[textDocument.uri]!!

        if (text != null) {
            assert(managedTextDoc.contents == text, {
                val change = Diff.buildChanges(managedTextDoc.contents, text)
                "Ground truth differed upon save!\n\n$change"
            })
        }
    }
}

data class ManagedTextDocument(var identifier: VersionedTextDocumentIdentifier, var contents: String)

fun rangeToStartEndOffset(doc: Document, range: Range): Pair<Int, Int> {
    val startLineOffset = doc.getLineStartOffset(range.start.line)
    val startOffset = startLineOffset + range.start.character
    val endLineOffset = doc.getLineStartOffset(range.end.line)
    val endOffset = endLineOffset + range.start.character

    return Pair(startOffset, endOffset)
}

fun applyChanges(doc: Document, contentChanges: List<TextDocumentContentChangeEvent>) =
    contentChanges.forEach { applyChange(doc, it) }

fun applyChange(doc: Document, change: TextEdit) {
    LOG.debug("Applying change: $change")
    val (startOffset, endOffset) = rangeToStartEndOffset(doc, change.range)
    doc.replaceString(startOffset, endOffset, change.newText)
}
fun applyChange(doc: Document, change: TextDocumentContentChangeEvent) {
    LOG.debug("Applying change: $change")
    if(change.range == null) {
        // Change is the full text of the document
        doc.setText(change.text)
    } else {
        val (startOffset, endOffset) = rangeToStartEndOffset(doc, change.range)
        doc.replaceString(startOffset, endOffset, change.text)
    }
}
