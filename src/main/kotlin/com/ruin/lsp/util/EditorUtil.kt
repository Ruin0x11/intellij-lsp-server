package com.ruin.lsp.util

import com.intellij.codeInsight.TargetElementUtil
import com.intellij.openapi.Disposable
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.EditorFactory
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.ruin.lsp.model.LogPrintWriter
import org.eclipse.lsp4j.Position

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
        LOG.error("Exception during editor callback: " + e.message)
        e.printStackTrace(LogPrintWriter(LOG))
    } finally {
        val editorFactory = EditorFactory.getInstance()
        editorFactory.releaseEditor(editor)
    }
}
