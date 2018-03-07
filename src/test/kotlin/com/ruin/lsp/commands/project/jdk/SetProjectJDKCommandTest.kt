package com.ruin.lsp.commands.project.jdk

import com.intellij.JavaTestUtil
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.projectRoots.ProjectJdkTable
import com.intellij.openapi.projectRoots.Sdk
import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.testFramework.PlatformTestCase
import com.ruin.lsp.model.invokeCommandAndWait
import com.ruin.lsp.util.getURIForFile
import kotlin.test.assertNotEquals

class SetProjectJDKCommandTest : PlatformTestCase() {
    //override val projectName = JAVA_PROJECT
    var jdk: Sdk? = null
    var jdkBefore: Sdk? = null
    var jdkAfter: Sdk? = null

    override fun setUp() {
        super.setUp()
        //jdk = JavaTestUtil.getTestJdk()
    }

    override fun initApplication() {
        super.initApplication()
        jdk = JavaTestUtil.getTestJdk()
    }

    private fun clearJdk() = ApplicationManager.getApplication().runWriteAction {
        val jdkTable = ProjectJdkTable.getInstance()

        val jdk = jdkTable.findMostRecentSdk { true }
        if (jdk != null) {
            jdkTable.removeJdk(jdk)
        }
    }


    fun `test sets jdk`() {
        val uri = getURIForFile(jdk!!.homeDirectory!!)
        val command = SetProjectJDKCommand(uri)

        clearJdk()

        jdkBefore = ProjectRootManager.getInstance(project).projectSdk
        val result = invokeCommandAndWait(command, project)
        jdkAfter = ProjectRootManager.getInstance(project).projectSdk

        assert(result)
        assertNotEquals(jdkBefore, jdkAfter)
        assertEquals(getURIForFile(jdkAfter!!.homeDirectory!!), uri)
    }

    override fun tearDown() {
        clearJdk()
        super.tearDown()
    }
}
