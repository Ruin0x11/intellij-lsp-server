package com.ruin.lsp

import com.intellij.JavaTestUtil
import com.intellij.JavaTestUtil.getTestJdk
import com.intellij.ide.impl.ProjectUtil
import com.intellij.openapi.Disposable
import com.intellij.openapi.project.ex.ProjectManagerEx
import com.intellij.openapi.roots.OrderRootType
import com.intellij.openapi.vfs.JarFileSystem
import com.intellij.psi.impl.compiled.ClsFileImpl
import com.intellij.testFramework.PlatformTestCase
import com.ruin.lsp.util.getURIForFile
import com.ruin.lsp.util.resolvePsiFromUri
import com.ruin.lsp.values.DocumentUri
import junit.framework.TestCase
import org.apache.commons.io.FileUtils
import org.apache.log4j.Logger
import java.io.File
import java.nio.file.Path
import java.io.IOException
import java.nio.file.FileSystems
import java.nio.file.Files
import java.util.zip.ZipFile


class GotoFileInsideJarTest : PlatformTestCase() {
    private var mockClient = MockClient()

    override fun initApplication() {
        super.initApplication()
        JavaTestUtil.setupTestJDK(testRootDisposable)
    }

    override fun setUp() {
        super.setUp()
        mockClient.setupTempDir()
    }

    override fun tearDown() {
        ProjectManagerEx.getInstanceEx().closeTestProject(project)
        super.tearDown()
    }

    private fun getJarUri(internalPath: String): DocumentUri {
        val jdk = getTestJdk()
        val jarUri = jdk.rootProvider.getUrls(OrderRootType.CLASSES).find { it.contains("rt.jar") }!!
        return "$jarUri$internalPath"
    }

    fun `test resolves a classes jar`() {
        val jarUri = getJarUri("java\\util\\ArrayList.class")
        val extractedFileUri = mockClient.visitJarFile(jarUri)
        val psi = resolvePsiFromUri(project, extractedFileUri, mockClient.tempDirUri)
        TestCase.assertNotNull(psi)
        TestCase.assertEquals(psi!!.name, "ArrayList.class")
        assert(psi is ClsFileImpl)
    }
}

/** Simulates client behavior of extracting the source file from the jar. */
private class MockClient {
    private val tempDir = File(System.getProperty("java.io.tmpdir"), "intellij-lsp-server-test")
    val tempDirUri = getURIForFile(tempDir)

    fun setupTempDir() {
        tempDir.mkdirs()
        FileUtils.cleanDirectory(tempDir)
    }

    fun visitJarFile(jarUri: DocumentUri): DocumentUri {
        val (jarFile, internalPath) = jarUri.split(JarFileSystem.JAR_SEPARATOR)
        val file = extractFileToTempdir(jarFile, internalPath)
        return getURIForFile(file)
    }

    private fun extractFileToTempdir(jarFilepath: String, internalPath: String): File {
        val jarFile = jarFilepath.replace("""jar://""".toRegex(), "").let { File(it) }
        val outDir = File(tempDir, "lsp-intellij")
        val jarDir = File(outDir, jarFile.nameWithoutExtension)
        jarDir.mkdirs()
        writeJarpathFile(jarDir, jarFile)
        val finalPath = File(jarDir, internalPath)
        finalPath.parentFile.mkdirs()
        extractFile(jarFile.toPath(), internalPath, finalPath.toPath())
        return finalPath
    }

    private fun writeJarpathFile(dest: File, jarFile: File) {
        val jarFileUri: DocumentUri = getURIForFile(jarFile)
        val jarpathDest = File(dest, "jarpath")
        jarpathDest.delete()
        jarpathDest.writeText(jarFileUri)
    }
}

val LOG = Logger.getLogger(GotoFileInsideJarTest::class.java)

@Throws(IOException::class)
fun extractFile(zipFile: Path, fileName: String, outputFile: Path) {
    ZipFile(zipFile.toString()).entries().iterator().forEachRemaining {
        LOG.error(it.name)
    }
    FileSystems.newFileSystem(zipFile, null).use({ fileSystem ->
        val fileToExtract = fileSystem.getPath(fileName)
        Files.copy(fileToExtract, outputFile)
    })
}
