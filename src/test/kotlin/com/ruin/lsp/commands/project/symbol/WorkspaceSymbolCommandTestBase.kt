package com.ruin.lsp.commands.project.symbol

import com.ruin.lsp.BaseTestCase
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.model.invokeCommandAndWait
import org.eclipse.lsp4j.SymbolInformation

abstract class WorkspaceSymbolCommandTestBase : BaseTestCase() {
    override val projectName: String
        get() = JAVA_PROJECT

    protected fun checkHasResult(query: String, expected: List<SymbolInformation>) {
        val command = WorkspaceSymbolCommand(query)
        val result = invokeCommandAndWait(command, project)
        assertSameElements(result.toList(), expected)
    }
}
