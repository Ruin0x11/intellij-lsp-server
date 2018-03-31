package com.ruin.lsp.commands.document.lens

import com.intellij.openapi.project.DumbService
import com.ruin.lsp.*
import com.ruin.lsp.commands.document.hover.HoverCommand
import com.ruin.lsp.model.invokeCommandAndWait
import org.eclipse.lsp4j.Position
import org.intellivim.FileEditingTestCase

abstract class CodeLensCommandTestBase : FileEditingTestCase() {
    override val projectName: String
        get() = TESTABLE_PROJECT

    override val filePath: String
        get() = TESTABLE_FILE_PATH

    protected fun checkRuns() {
        val command = CodeLensCommand()
        val result = invokeCommandAndWait(command, project, psiFile)
        print("dood")
    }
}
