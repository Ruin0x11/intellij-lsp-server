package com.ruin.lsp.commands.diagnostics

import com.intellij.codeInsight.daemon.impl.HighlightInfo
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.Editor
import com.intellij.psi.PsiFile
import org.eclipse.lsp4j.PublishDiagnosticsParams

class DiagnosticsThread(val file: PsiFile, val editor: Editor) : Runnable {
    var infos: List<HighlightInfo>? = null

    override fun run() {
        infos = getHighlights(file, editor)
    }
}
