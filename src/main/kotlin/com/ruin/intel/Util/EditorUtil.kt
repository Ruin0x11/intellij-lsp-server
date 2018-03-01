package com.ruin.intel.Util

import com.intellij.codeInsight.TargetElementUtil
import com.intellij.psi.PsiElement
import com.intellij.openapi.editor.Editor

/**
 * Convenience. Sometimes <code>PsiFile.findElementAt()</code>
 *  isn't enough. This will never return null.
 * @throws IllegalArgumentException if it couldn't find an element
 */
fun findTargetElement(editor: Editor): PsiElement {
    return TargetElementUtil
        .findTargetElement(editor,
            TargetElementUtil.getInstance().allAccepted) ?: throw IllegalArgumentException("No element under the cursor")
}
