package com.ruin.lsp.util

import com.intellij.codeInsight.TargetElementUtil
import com.intellij.diff.comparison.ComparisonManager
import com.intellij.diff.comparison.ComparisonPolicy
import com.intellij.diff.fragments.DiffFragment
import com.intellij.openapi.Disposable
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.editor.LogicalPosition
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.progress.DumbProgressIndicator
import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.util.Disposer
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.impl.PsiDocumentManagerBase
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.TextEdit

/**
 * Convenience. Sometimes <code>PsiFile.findElementAt()</code>
 *  isn't enough. This will never return null.
 * @throws IllegalArgumentException if it couldn't find an element
 */
fun ensureTargetElement(editor: Editor): PsiElement =
    findTargetElement(editor) ?: throw IllegalArgumentException("No element under the cursor")

fun findTargetElement(editor: Editor): PsiElement? =
    TargetElementUtil
        .findTargetElement(editor,
            TargetElementUtil.getInstance().allAccepted)

private val LOG = Logger.getInstance("#com.ruin.lsp.util.EditorUtil")

/**
 * Creates, uses, then releases an editor.
 *
 * For some reason the Disposer doesn't release editor instances, so this is just working around the resulting
 * boilerplate.
 */
fun withEditor(context: Disposable, file: PsiFile, position: Position = Position(0, 0), callback: (Editor) -> Unit) {
    val editor = createEditor(context, file, position)

    try {
        callback(editor)
    } catch (e: Exception) {
        LOG.error("Exception during editor callback: " + e
            + e.stackTrace.asList().joinToString("\n") { it.toString() }
        )
    } finally {
        val editorFactory = EditorFactory.getInstance()
        editorFactory.releaseEditor(editor)
    }
}

fun withEditor(context: Disposable, file: PsiFile, offset: Int, callback: (Editor) -> Unit) {
    val editor = createEditor(context, file, offset)

    try {
        callback(editor)
    } catch (e: Exception) {
        LOG.error("Exception during editor callback: " + e
            + e.stackTrace.asList().joinToString("\n") { it.toString() }
        )
    } finally {
        val editorFactory = EditorFactory.getInstance()
        editorFactory.releaseEditor(editor)
    }
}

/**
 * Gathers a list of TextEdits by copying a PsiFile, running an operation on the copy and diffing the results.
 *
 * All operations on a PsiFile should be performed on the copy inside the callback, not the original.
 * @param file file to run action on
 * @param callback function run on the copied file and an editor for the copy
 */
fun differenceFromAction(file: PsiFile, callback: (Editor, PsiFile) -> Unit): List<TextEdit>? {
    val doc = getDocument(file)!!
    PsiDocumentManagerBase.getInstance(file.project).commitDocument(doc)
    val copy = file.copy() as PsiFile
    assert(file.text == copy.text)
    withEditor(Disposer.newDisposable(), copy, Position(0, 0)) { editor ->
        callback(editor, copy)
    }

    val oldDoc = getDocument(file) ?: return null
    val newDoc = getDocument(copy) ?: return null
    LOG.debug("=== Old doc:\n${oldDoc.text}")
    LOG.debug("=== New doc:\n${newDoc.text}")
    return textEditFromDocs(oldDoc, newDoc)
}

fun DiffFragment.toTextEdit(oldDoc: Document, newDoc: Document): TextEdit {
    val start = offsetToPosition(oldDoc, this.startOffset1)
    val end = offsetToPosition(oldDoc, this.endOffset1)
    val text = newDoc.getText(TextRange(this.startOffset2, this.endOffset2))
    return TextEdit(Range(start, end), text)
}

fun textEditFromDocs(oldDoc: Document, newDoc: Document): List<TextEdit> {
    val changes = diff(oldDoc.text, newDoc.text)
    return changes.map { it.toTextEdit(oldDoc, newDoc) }
}

fun diff(old: String, new: String): MutableList<DiffFragment> {
    val indicator = ProgressManager.getInstance().progressIndicator ?: DumbProgressIndicator.INSTANCE
    val changes = ComparisonManager.getInstance().compareChars(old, new, ComparisonPolicy.DEFAULT, indicator)
    LOG.debug("=== Diff:\n${changes.joinToString("\n") { it.toString() }}")
    return changes
}

fun LogicalPosition.position() = Position(line, column)
