package com.ruin.lsp.commands.diagnostics

import com.intellij.codeInsight.daemon.impl.HighlightInfo
import com.intellij.ide.impl.ProjectUtil
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Disposer
import com.intellij.openapi.util.Ref
import com.intellij.psi.PsiFile
import com.intellij.testFramework.UsefulTestCase
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.util.*
import org.eclipse.lsp4j.Diagnostic
import com.intellij.testFramework.fixtures.impl.LightTempDirTestFixtureImpl
import com.intellij.testFramework.fixtures.JavaTestFixtureFactory
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor
import com.intellij.testFramework.fixtures.IdeaTestFixtureFactory
import com.intellij.testFramework.fixtures.JavaCodeInsightTestFixture
import com.ruin.lsp.BaseTestCase


abstract class DiagnosticsCommandTestBase : BaseTestCase() {
    override val projectName: String
        get() = JAVA_PROJECT

    protected fun checkDiagnosticsFound(filePath: String, expected: List<Diagnostic>) {
        val file = getVirtualFile(project, filePath)
        val (_, psiFile) = ensurePsiFromUri(file.url)
        val doc = getDocument(file.url)!!

        val thread = DiagnosticsThread(psiFile, doc, null)
        ApplicationManager.getApplication().executeOnPooledThread(thread).get()

        assert(thread.diags != null)
        assertSameElements(thread.diags!!, expected)
    }
}
