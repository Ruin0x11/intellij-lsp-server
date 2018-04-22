package com.ruin.lsp.commands.document.lens

import com.ruin.lsp.model.invokeCommandAndWait
import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase
import com.ruin.lsp.util.getDocument
import org.eclipse.lsp4j.CodeLens
import com.intellij.codeInsight.daemon.GutterMark




abstract class CodeLensCommandTestBase : LightCodeInsightFixtureTestCase() {
    protected fun doTest(name: String, code: String, expected: List<CodeLens>) {
        myFixture.configureByText(name, code)
        val command = CodeLensCommand()
        val result = invokeCommandAndWait(command, project, myFixture.file)

        assertEquals(expected, result.toList())
    }
}
