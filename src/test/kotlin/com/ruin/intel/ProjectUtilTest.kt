package com.ruin.intel

import com.ruin.intel.util.getPsiFile
import com.ruin.intel.util.resolvePsiFromUri
import java.io.File


class ProjectUtilTest : BaseTestCase() {

    override val projectName: String
        get() = TESTABLE_PROJECT

    fun `test finds project`() {
        val project = com.ruin.intel.util.getProject(getProjectIml())
        assertNotNull(project)
        assertEquals(TESTABLE_PROJECT, project!!.name)
    }

    fun `test doesn't find nonexistent project`() {
        val project = com.ruin.intel.util.getProject("blah")
        assertNull(project)
    }

    fun `test resolves PsiFile from URI`() {
        val expectedTarget = getPsiFile(getProject(), "src/org/intellivim/runnable/test/Testable.java")
        val uri = File(getProjectPath(), "src/org/intellivim/runnable/test/Testable.java").toURI().toURL()
        val pair = resolvePsiFromUri(uri.toString())
        assertEquals(pair!!.second, expectedTarget)
    }
}

