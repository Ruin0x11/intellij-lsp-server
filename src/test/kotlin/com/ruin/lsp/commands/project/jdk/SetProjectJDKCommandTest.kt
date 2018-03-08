package com.ruin.lsp.commands.project.jdk

import com.intellij.JavaTestUtil
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.projectRoots.ProjectJdkTable
import com.intellij.openapi.projectRoots.Sdk
import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.testFramework.PlatformTestCase
import com.ruin.lsp.model.JdkKind
import com.ruin.lsp.model.invokeCommandAndWait
import com.ruin.lsp.util.getURIForFile
import org.jetbrains.idea.devkit.projectRoots.IdeaJdk
import org.jetbrains.idea.devkit.projectRoots.Sandbox
import kotlin.test.assertNotEquals

class SetProjectJDKCommandTest : PlatformTestCase() {
    //override val projectName = JAVA_PROJECT
    var jdk: Sdk? = null
    var jdkBefore: Sdk? = null
    var jdkAfter: Sdk? = null

    override fun initApplication() {
        super.initApplication()
        clearJdk()
        jdk = null
        jdkBefore = null
        jdkAfter = null
        jdk = JavaTestUtil.getTestJdk()
    }

    private fun clearJdk() = ApplicationManager.getApplication().runWriteAction {
        val jdkTable = ProjectJdkTable.getInstance()

        jdkTable.allJdks.forEach { jdk ->
            if (jdk != null) {
                jdkTable.removeJdk(jdk)
            }
        }
    }

    fun `test sets jdk`() {
        val uri = getURIForFile(jdk!!.homeDirectory!!)
        val command = SetProjectJDKCommand(uri, JdkKind.JDK)

        jdkBefore = ProjectRootManager.getInstance(project).projectSdk
        val result = invokeCommandAndWait(command, project)
        jdkAfter = ProjectRootManager.getInstance(project).projectSdk

        assert(result.success)
        assertNotNull(jdkAfter)
        assertNotEquals(jdkBefore, jdkAfter)
        assertEquals(getURIForFile(jdkAfter!!.homeDirectory!!), uri)
    }

    fun `test finds existing jdk`() {
        val uri = getURIForFile(jdk!!.homeDirectory!!)
        val command = SetProjectJDKCommand(uri, JdkKind.JDK)

        invokeCommandAndWait(command, project)
        invokeCommandAndWait(command, project)
        val result = invokeCommandAndWait(command, project)

        assert(result.success)
        assertEquals(ProjectJdkTable.getInstance().allJdks.size, 1)
    }

    fun `test sets idea jdk`() {
        val uri = getURIForFile(jdk!!.homeDirectory!!)
        val command = SetProjectJDKCommand(uri, JdkKind.INTELLIJ)

        jdkBefore = ProjectRootManager.getInstance(project).projectSdk
        val result = invokeCommandAndWait(command, project)
        jdkAfter = ProjectRootManager.getInstance(project).projectSdk

        assert(result.success)
        assertNotNull(jdkAfter)
        assertNotEquals(jdkBefore, jdkAfter)
        assert(jdkAfter!!.sdkType == IdeaJdk.getInstance())
        assertEquals(jdkAfter!!.homePath, IdeaJdk().suggestHomePath())

        val data = jdkAfter!!.sdkAdditionalData as? Sandbox
        assertNotNull(data)
        val innerJdk = data!!.javaSdk
        assertNotNull(innerJdk)
        assertEquals(getURIForFile(innerJdk!!.homeDirectory!!), uri)
    }

    fun `test finds existing idea jdk`() {
        val uri = getURIForFile(jdk!!.homeDirectory!!)
        val command = SetProjectJDKCommand(uri, JdkKind.INTELLIJ)

        invokeCommandAndWait(command, project)
        invokeCommandAndWait(command, project)
        val result = invokeCommandAndWait(command, project)

        assert(result.success)
        assertEquals(ProjectJdkTable.getInstance().allJdks.size, 2)
    }

    override fun tearDown() {
        clearJdk()
        super.tearDown()
    }
}
