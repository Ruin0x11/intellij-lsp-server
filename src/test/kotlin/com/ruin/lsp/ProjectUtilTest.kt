package com.ruin.lsp

import com.ruin.lsp.util.*
import com.ruin.lsp.values.DocumentUri
import java.io.File
import kotlin.test.assertEquals


class ProjectUtilTest : BaseTestCase() {

    override val projectName: String
        get() = TESTABLE_PROJECT

    fun `test finds project`() {
        val project = com.ruin.lsp.util.getProject(getProjectPath())
        assertNotNull(project)
        assertEquals(TESTABLE_PROJECT, project!!.name)
    }

    fun `test doesn't find nonexistent project`() {
        val project = com.ruin.lsp.util.getProject("blah")
        assertNull(project)
    }

    fun `test resolves PsiFile from URI`() {
        val expectedTarget = getPsiFile(project, TESTABLE_FILE_PATH)
        val uri = getURIForFile(File(getProjectPath(), TESTABLE_FILE_PATH))
        val file = resolvePsiFromUri(project, uri)
        assertEquals(expectedTarget, file)
    }

    fun `test normalizes URIs`() {
        val expected = "file:///E:/Program Files/test.txt"
        val cases = listOf(
            "file:/e:/Program Files/test.txt",
            "file://e:/Program Files/test.txt",
            "file://E:\\Program Files\\test.txt",
            "file:///E:/Program Files/test.txt",
            "file:///E:/Program%20Files/test.txt",
            "file:///E:/Program%20Files/test.txt/"
        ).map(::normalizeUri)

        cases.forEach {
            assertEquals(expected, it)
        }
    }

    fun `test converts URI to path`() {
        val expected = "E:/Program Files/test.txt"
        val cases = listOf(
            "file:/e:/Program Files/test.txt",
            "file://e:/Program Files/test.txt",
            "file://E:\\Program Files\\test.txt",
            "file:///E:/Program Files/test.txt",
            "file:///E:/Program%20Files/test.txt"
        ).map(::uriToPath)

        cases.forEach {
            assertEquals(expected, it)
        }
    }

    fun `test converts unix URI to path`() {
        val expected = "/home/ruin/My Folder/test.txt"
        val cases = listOf(
            "file://home/ruin/My Folder/test.txt",
            "file:///home/ruin/My Folder/test.txt",
            "file:///home/ruin/My%20Folder/test.txt"
        ).map(::uriToPath)

        cases.forEach {
            assertEquals(expected, it)
        }
    }


    fun `test converts file extracted from jar to internal source dir`() {
        val tempDir = "file:///tmp"
        val expected: Pair<String, DocumentUri> = Pair("org/ruin/stuff/MyClass.class", "file:///tmp/lsp-intellij/my-library/jarpath")

        val cases = listOf(
            "file:///tmp/lsp-intellij/my-library/org/ruin/stuff/MyClass.class",
            "file://tmp/lsp-intellij/my-library/org/ruin/stuff/MyClass.class"
        ).map { jarExtractedFileToJarpathFile(it, tempDir) }

        cases.forEach {
            assertEquals(expected, it)
        }
    }


    fun `test converts file extracted from jar to internal source dir on windows`() {
        val tempDir = "file:///E:/temp"
        val expected: Pair<String, DocumentUri> = Pair("org/ruin/stuff/MyClass.class", "file:///E:/temp/lsp-intellij/my-library/jarpath")

        val cases = listOf(
            "file:///E:/temp/lsp-intellij/my-library/org/ruin/stuff/MyClass.class",
            "file://E:/temp/lsp-intellij/my-library/org/ruin/stuff/MyClass.class"
        ).map { jarExtractedFileToJarpathFile(it, tempDir) }

        cases.forEach {
            assertEquals(expected, it)
        }
    }
}

