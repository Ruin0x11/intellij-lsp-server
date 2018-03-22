package com.ruin.lsp

import com.intellij.ide.impl.ProjectUtil
import com.ruin.lsp.util.getURIForFile
import com.ruin.lsp.util.getPsiFile
import com.ruin.lsp.util.resolveProjectFromUri
import com.ruin.lsp.util.resolvePsiFromUri
import java.io.File

class ProjectUtilMavenMultiModuleTest : BaseTestCase() {

    override val projectName: String
        get() = MAVEN_MULTI_MODULE_PROJECT

    fun `test finds project`() {
        val project = com.ruin.lsp.util.getProject(getProjectIml())
        assertNotNull(project)
        assertEquals(MAVEN_MULTI_MODULE_PROJECT, project!!.name)
        ProjectUtil.closeAndDispose(project)
    }

    fun `test resolves PsiFile from URI`() {
        val expectedTarget = getPsiFile(project, MAVEN_MULTI_MODULE_APP_PATH)
        val uri = getURIForFile(File(getProjectPath(), MAVEN_MULTI_MODULE_APP_PATH))
        val pair = resolvePsiFromUri(uri)
        assertEquals(expectedTarget, pair!!.second)
    }

    fun `test resolves root project from URI under child project`() {
        val expectedTarget = MAVEN_MULTI_MODULE_PROJECT
        val uri = getURIForFile(File(getProjectPath(), MAVEN_MULTI_MODULE_APP_PATH))
        val pair = resolveProjectFromUri(uri)
        assertEquals(expectedTarget, pair!!.first.name)
    }
}

