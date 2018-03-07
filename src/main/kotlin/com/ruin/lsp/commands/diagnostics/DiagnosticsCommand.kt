package com.ruin.lsp.commands.diagnostics

import com.intellij.codeInsight.daemon.impl.HighlightInfo
import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.openapi.editor.Editor
import com.ruin.lsp.commands.Command
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.commands.highlight.offsetToPosition
import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.DiagnosticSeverity
import org.eclipse.lsp4j.PublishDiagnosticsParams
import org.eclipse.lsp4j.Range


class DiagnosticsCommand : Command<PublishDiagnosticsParams> {
    override fun execute(ctx: ExecutionContext): PublishDiagnosticsParams =
        PublishDiagnosticsParams(ctx.uri, listOf())
}
