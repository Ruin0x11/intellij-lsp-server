package com.ruin.lsp.util

import com.intellij.codeInsight.TargetElementUtil
import com.intellij.openapi.Disposable
import com.intellij.psi.PsiElement
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.EditorFactory
import com.intellij.psi.PsiFile
import com.ruin.lsp.values.Position

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


/**
 * Creates, uses, then releases an editor.
 *
 * For some reason the Disposer doesn't release editor instances, so this is just working around the resulting
 * boilerplate.
 */
fun withEditor(context: Disposable, file: PsiFile, position: Position, callback: (Editor) -> Unit) {
    val editor = createEditor(context, file, position)

    callback(editor)

    val editorFactory = EditorFactory.getInstance()
    editorFactory.releaseEditor(editor)
}
