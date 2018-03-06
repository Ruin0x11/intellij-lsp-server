package com.ruin.lsp.commands.diagnostics

import com.ruin.lsp.PROBLEMATIC_FILE_PATH
import com.ruin.lsp.range
import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.DiagnosticSeverity

class DiagnosticsCommandTestCase : DiagnosticsCommandTestBase() {
    fun `test finds missing imports`() = checkDiagnosticsFound(PROBLEMATIC_FILE_PATH,
        listOf(
            Diagnostic(range(0, 0, 1, 1), "dsaf", DiagnosticSeverity.Error, "intellij"),
            Diagnostic(range(0, 0, 1, 1), "dsaf", DiagnosticSeverity.Error, "intellij")
        ))
}
