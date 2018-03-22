package com.ruin.lsp.commands.document.diagnostics

import com.ruin.lsp.PROBLEMATIC_FILE_PATH
import com.ruin.lsp.range
import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.DiagnosticSeverity

class DiagnosticsCommandTestCase : DiagnosticsCommandTestBase() {
    fun `test finds missing imports`() = checkDiagnosticsFound(PROBLEMATIC_FILE_PATH,
        listOf(
            Diagnostic(range(8, 8, 8, 19),
                "Cannot resolve symbol 'NotImported'", DiagnosticSeverity.Error, "intellij"),
            Diagnostic(range(9, 8, 9, 23),
                "Cannot resolve symbol 'AlsoNotImported'", DiagnosticSeverity.Error, "intellij")
        ))
}
