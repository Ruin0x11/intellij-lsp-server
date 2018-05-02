package com.ruin.lsp.commands.document.diagnostics

import com.ruin.lsp.KOTLIN_PROJECT
import com.ruin.lsp.PROBLEMATIC_FILE_PATH
import com.ruin.lsp.forKotlin
import com.ruin.lsp.range
import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.DiagnosticSeverity

class DiagnosticsCommandTestCaseKt : DiagnosticsCommandTestBase() {
    override val projectName: String
        get() = KOTLIN_PROJECT

//    fun `test finds missing imports`() = checkDiagnosticsFound(forKotlin(PROBLEMATIC_FILE_PATH),
//        listOf(
//                Diagnostic(range(8, 12, 8, 15),
//            "[UNUSED_VARIABLE] Variable 'obj' is never used", DiagnosticSeverity.Warning, "intellij"),
//        Diagnostic(range(8, 17, 8, 28),
//            "[UNRESOLVED_REFERENCE] Unresolved reference: NotImported", DiagnosticSeverity.Error, "intellij"),
//            Diagnostic(range(9, 12, 9, 17),
//            "[UNUSED_VARIABLE] Variable 'other' is never used", DiagnosticSeverity.Warning, "intellij"),
//        Diagnostic(range(9, 19, 9, 34),
//            "[UNRESOLVED_REFERENCE] Unresolved reference: AlsoNotImported", DiagnosticSeverity.Error, "intellij")
//        ))
}
