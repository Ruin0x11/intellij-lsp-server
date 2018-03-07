package com.ruin.lsp

import com.ruin.lsp.util.*
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
        val expected = "file:///e:/Program Files/test.txt"
        val cases = listOf(
            "file:/e:/Program Files/test.txt",
            "file://e:/Program Files/test.txt",
            "file://E:\\Program Files\\test.txt",
            "file:///E:/Program Files/test.txt"
        ).map(::normalizeUri)

        cases.forEach {
            assert(expected.equals(it, true), {
            "Expected: $expected\n" +
                "Got: $it"
        }) }
    }

    fun `test converts URI to path`() {
        val expected = "e:/Program Files/test.txt"
        val cases = listOf(
            "file:/e:/Program Files/test.txt",
            "file://e:/Program Files/test.txt",
            "file://E:\\Program Files\\test.txt",
            "file:///E:/Program Files/test.txt"
        ).map(::uriToPath)

        cases.forEach {
            assert(expected.equals(it, true), {
                "Expected: $expected\n" +
                    "Got: $it"
            }) }
    }
}

