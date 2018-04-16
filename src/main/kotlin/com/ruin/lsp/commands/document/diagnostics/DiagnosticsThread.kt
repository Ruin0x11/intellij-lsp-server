package com.ruin.lsp.commands.document.diagnostics

import com.intellij.codeInsight.daemon.impl.DaemonCodeAnalyzerEx
import com.intellij.codeInsight.daemon.impl.DaemonProgressIndicator
import com.intellij.codeInsight.daemon.impl.HighlightInfo
import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.openapi.Disposable
import com.intellij.openapi.application.ReadAction
import com.intellij.openapi.editor.Document
import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.project.IndexNotReadyException
import com.intellij.openapi.util.Computable
import com.intellij.openapi.util.Disposer
import com.intellij.psi.PsiFile
import com.ruin.lsp.util.getURIForFile
import com.ruin.lsp.util.offsetToPosition
import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.DiagnosticSeverity
import org.eclipse.lsp4j.PublishDiagnosticsParams
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.services.LanguageClient

/**
 * Runnable for computing and publishing diagnostics.
 *
 * Unlike the other commands, this has to be run using executeOnPooledThread instead of on the EDT.
 */
class DiagnosticsThread(val file: PsiFile, val document: Document, val client: LanguageClient?) : Runnable {
    var diags: List<Diagnostic>? = null

    override fun run() {
        return
        // Wait in case the user is updating a lot of text at once
        try {
            Thread.sleep(1000)
        } catch (e: InterruptedException) {

        }

        val infos = getHighlights(file, document)
        diags = infos.mapNotNull { it.toDiagnostic(document) }
        client?.publishDiagnostics(PublishDiagnosticsParams(getURIForFile(file), diags))
    }
}

fun HighlightInfo.diagnosticSeverity() =
    when (this.severity) {
        HighlightSeverity.INFORMATION -> DiagnosticSeverity.Information
        HighlightSeverity.WARNING -> DiagnosticSeverity.Warning
        HighlightSeverity.ERROR -> DiagnosticSeverity.Error
        else -> DiagnosticSeverity.Hint
    }

fun HighlightInfo.toDiagnostic(doc: Document): Diagnostic? {
    if (this.description == null)
        return null

    val description = this.description
    val start = offsetToPosition(doc, this.getStartOffset())
    val end = offsetToPosition(doc, this.getEndOffset())

    return Diagnostic(Range(start, end), description, this.diagnosticSeverity(), "intellij")
}

fun getHighlights(file: PsiFile, doc: Document): List<HighlightInfo> {
    val disposable = Disposer.newDisposable()
    return try {
        doHighlighting(disposable, doc, file)
    } catch (e: RuntimeException) {
        // we're not trying to suppress the error;
        //  we just want to make sure we can clean up after ourselves
        throw e
    } finally {
        Disposer.dispose(disposable)
    }
}

fun doHighlighting(context: Disposable,
                   doc: Document, psiFile: PsiFile): List<HighlightInfo> {

    val progress = DaemonProgressIndicator()

    Disposer.register(context, progress)

    val project = psiFile.project

    return ProgressManager.getInstance().runProcess(Computable {
        val analyzer = DaemonCodeAnalyzerEx.getInstanceEx(project)

        // ensure we get fresh results; the restart also seems to
        //  prevent the "process canceled" issue (see #30)
        //PsiDocumentManager.getInstance(document).commitAllDocuments()

        ReadAction.compute<List<HighlightInfo>, Exception> {
            // analyze!
            try {
                analyzer.restart(psiFile)
                analyzer.runMainPasses(
                    psiFile, doc, progress)
            } catch (e: IndexNotReadyException) {
                listOf()
            }
        }

    }, progress)
}
