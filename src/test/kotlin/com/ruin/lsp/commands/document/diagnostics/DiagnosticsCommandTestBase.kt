package com.ruin.lsp.commands.document.diagnostics

import com.intellij.openapi.application.ApplicationManager
import com.ruin.lsp.BaseTestCase
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.util.ensurePsiFromUri
import com.ruin.lsp.util.getDocument
import com.ruin.lsp.util.getVirtualFile
import org.eclipse.lsp4j.Diagnostic


abstract class DiagnosticsCommandTestBase : BaseTestCase() {
    protected fun checkDiagnosticsFound(filePath: String, expected: List<Diagnostic>) {
        val file = getVirtualFile(project, filePath)
        val psiFile = ensurePsiFromUri(project, file.url)
        val doc = getDocument(project, file.url)!!

        val thread = DiagnosticsThread(psiFile, doc, null)
        ApplicationManager.getApplication().executeOnPooledThread(thread).get()

        assert(thread.diags != null)
        assertSameElements(thread.diags!!, expected)
    }
}
