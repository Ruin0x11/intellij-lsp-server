package com.ruin.lsp.commands.diagnostics

import com.intellij.codeInsight.daemon.impl.DaemonCodeAnalyzerEx
import com.intellij.codeInsight.daemon.impl.DaemonProgressIndicator
import com.intellij.codeInsight.daemon.impl.HighlightInfo
import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.openapi.Disposable
import com.intellij.openapi.application.ReadAction
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.ex.EditorEx
import com.intellij.openapi.fileEditor.impl.text.AsyncHighlighterUpdater
import com.intellij.openapi.util.Computable
import com.intellij.openapi.util.Disposer
import com.intellij.openapi.util.Ref
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiFile
import com.ruin.lsp.commands.Command
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.commands.highlight.offsetToPosition
import com.ruin.lsp.util.createEditor
import com.ruin.lsp.util.invokeAndWaitIfNeeded
import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.DiagnosticSeverity
import org.eclipse.lsp4j.PublishDiagnosticsParams
import org.eclipse.lsp4j.Range
import com.intellij.openapi.progress.ProgressManager




class DiagnosticsCommand : Command<PublishDiagnosticsParams> {
    override fun execute(ctx: ExecutionContext): PublishDiagnosticsParams =
        PublishDiagnosticsParams(ctx.uri, listOf())
}

fun HighlightInfo.diagnosticSeverity() =
    when (this.severity) {
        HighlightSeverity.INFORMATION -> DiagnosticSeverity.Information
        HighlightSeverity.WARNING -> DiagnosticSeverity.Warning
        HighlightSeverity.ERROR -> DiagnosticSeverity.Error
        else -> DiagnosticSeverity.Hint
    }

fun HighlightInfo.toDiagnostic(editor: Editor): Diagnostic? {
    if (this.description == null)
        return null

    val description = this.description
    val start = offsetToPosition(editor, this.getStartOffset())
    val end = offsetToPosition(editor, this.getEndOffset())

    return Diagnostic(Range(start, end), description, this.diagnosticSeverity(), "intellij")
}

fun getHighlights(file: PsiFile, editor: Editor): List<HighlightInfo> {
    val disposable = Disposable {  }
    return try {
        doHighlighting(disposable, editor, file)
    } catch (e: RuntimeException) {
        // we're not trying to suppress the error;
        //  we just want to make sure we can clean up after ourselves
        throw e
    } finally {
        Disposer.dispose(disposable)
    }
}

fun doHighlighting(context: Disposable,
                   editor: Editor, psiFile: PsiFile): List<HighlightInfo> {

    val progress = DaemonProgressIndicator()
    Disposer.register(context, progress)
    val project = psiFile.project
    val analyzer = DaemonCodeAnalyzerEx.getInstanceEx(project)

    return ProgressManager.getInstance().runProcess(Computable {
        val project = psiFile.project
        val analyzer = DaemonCodeAnalyzerEx.getInstanceEx(project)

        // ensure we get fresh results; the restart also seems to
        //  prevent the "process canceled" issue (see #30)
        //PsiDocumentManager.getInstance(project).commitAllDocuments()

        ReadAction.compute<List<HighlightInfo>, Exception> {
            // analyze!
            analyzer.restart(psiFile)
            analyzer.runMainPasses(
                psiFile, editor.document, progress)
        }

    }, progress)
}
