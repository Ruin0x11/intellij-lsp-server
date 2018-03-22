package com.ruin.lsp

import com.ruin.lsp.util.getURIForFile
import com.ruin.lsp.util.getPsiFile
import com.ruin.lsp.util.resolvePsiFromUri
import java.io.File


class ProjectUtilMultiModuleTest : BaseTestCase() {

    override val projectName: String
        get() = MULTI_MODULE_PROJECT

    fun `test finds project`() {
        val project = com.ruin.lsp.util.getProject(getProjectIml())
        assertNotNull(project)
        assertEquals(MULTI_MODULE_PROJECT, project!!.name)
    }

    fun `test resolves PsiFile from URI`() {
        val expectedTarget = getPsiFile(project, MULTI_MODULE_APP_PATH)
        val uri = getURIForFile(File(getProjectPath(), MULTI_MODULE_APP_PATH))
        val pair = resolvePsiFromUri(uri)
        assertEquals(expectedTarget, pair!!.second)
    }
}

