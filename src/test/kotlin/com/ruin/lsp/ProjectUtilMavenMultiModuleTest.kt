package com.ruin.lsp

import com.ruin.lsp.util.fileToUri
import com.ruin.lsp.util.getPsiFile
import com.ruin.lsp.util.resolvePsiFromUri
import java.io.File

class ProjectUtilMavenMultiModuleTest : BaseTestCase() {

    override val projectName: String
        get() = MAVEN_MULTI_MODULE_PROJECT

    fun `test finds project`() {
        val project = com.ruin.lsp.util.getProject(getProjectIml())
        assertNotNull(project)
        assertEquals(MAVEN_MULTI_MODULE_PROJECT, project!!.name)
    }

    fun `test resolves PsiFile from URI`() {
        val expectedTarget = getPsiFile(project, MAVEN_MULTI_MODULE_APP_PATH)
        val uri = fileToUri(File(getProjectPath(), MAVEN_MULTI_MODULE_APP_PATH))
        val pair = resolvePsiFromUri(uri)
        assertEquals(pair!!.second, expectedTarget)
    }
}

