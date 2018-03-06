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


val projectRef = Ref<Project>()


abstract class DiagnosticsCommandTestBase : BaseTestCase() {
    protected var myFixture: JavaCodeInsightTestFixture? = null

    override val projectName: String
        get() = JAVA_PROJECT

    protected fun checkDiagnosticsFound(filePath: String, expected: List<Diagnostic>) {

        val project = ensureProject("projects/" + JAVA_PROJECT + "/" + JAVA_PROJECT + ".iml")
        val file = getVirtualFile(project, filePath)
        ProjectUtil.closeAndDispose(project)
        val (projectb, psiFile) = ensurePsiFromUri(file.url)
        val doc = getDocument(file.url)!!
        val disposable = Disposer.newDisposable()
        val editor = createEditor(disposable, psiFile)
        val thing = DiagnosticsThread(psiFile, editor)
        ApplicationManager.getApplication().executeOnPooledThread(thing).get()

        assert(thing.infos != null)

        EditorFactory.getInstance().releaseEditor(editor)
    }

}
