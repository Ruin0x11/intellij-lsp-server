package com.ruin.lsp

import com.ruin.lsp.util.fileToUri
import com.ruin.lsp.util.getPsiFile
import com.ruin.lsp.util.normalizeUri
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
        val uri = fileToUri(File(getProjectPath(), TESTABLE_FILE_PATH))
        val pair = resolvePsiFromUri(uri)
        assertEquals(pair!!.second, expectedTarget)
    }

    fun `test normalizes URIs`() {
        val expected = "file:///e:/build/intellij-lsp-server/build.gradle.kts"
        val cases = listOf(
            "file:/e:/build/intellij-lsp-server/build.gradle.kts",
            "file://e:/build/intellij-lsp-server/build.gradle.kts",
            "file://E:\\build\\intellij-lsp-server\\build.gradle.kts",
            "file:///E:/build/intellij-lsp-server/build.gradle.kts"
        )

        cases.forEach {
            val uri = normalizeUri(it)
            assert(expected.equals(uri, true), {
            "Expected: $expected\n" +
                "Got: $uri"
        }) }
    }
}

