package com.ruin.lsp

import com.ruin.lsp.util.getPsiFile
import com.ruin.lsp.util.resolvePsiFromUri
import java.io.File


class ProjectUtilTest : BaseTestCase() {

    override val projectName: String
        get() = TESTABLE_PROJECT

    fun `test finds project`() {
        val project = com.ruin.lsp.util.getProject(getProjectIml())
        assertNotNull(project)
        assertEquals(TESTABLE_PROJECT, project!!.name)
    }

    fun `test doesn't find nonexistent project`() {
        val project = com.ruin.lsp.util.getProject("blah")
        assertNull(project)
    }

    fun `test resolves PsiFile from URI`() {
        val expectedTarget = getPsiFile(project, TESTABLE_FILE_PATH)
        val uri = File(getProjectPath(), TESTABLE_FILE_PATH).toURI().toURL()
        val pair = resolvePsiFromUri(uri.toString())
        assertEquals(pair!!.second, expectedTarget)
    }
}

