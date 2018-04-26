package org.intellivim

import com.intellij.ide.impl.ProjectUtil
import com.intellij.idea.IdeaTestApplication
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.application.WriteAction
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.project.ProjectManager
import com.intellij.openapi.project.ex.ProjectManagerEx
import com.intellij.openapi.project.impl.ProjectManagerImpl
import com.intellij.openapi.util.Disposer
import com.intellij.openapi.util.ThrowableComputable
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiFile
import com.intellij.testFramework.RunAll
import com.ruin.lsp.BaseTestCase
import com.ruin.lsp.util.getDocument
import com.ruin.lsp.util.getPsiFile
import com.ruin.lsp.util.getVirtualFile
import junit.framework.TestCase
import org.eclipse.lsp4j.TextDocumentItem
import org.eclipse.lsp4j.VersionedTextDocumentIdentifier
import java.io.FileNotFoundException
import java.io.IOException

/**
 * Convenient base test case for commands that
 * will edit some file. Any changes will be
 * reverted after each test, and assertions
 * are provided to make testing easier
 *
 * @author dhleong
 */
abstract class FileEditingTestCase : BaseTestCase() {

    private var originalContents: ByteArray? = null
    private var psiOriginalContents: ByteArray? = null


    protected abstract val filePath: String

    protected val file: VirtualFile
        get() {
            val proj = project
            return getVirtualFile(proj, filePath)
        }

    protected val psiFile: PsiFile
        get() {
            val proj = project
            return getPsiFile(proj, filePath)!!
        }

    private val currentFileContentsSafely: String?
        get() {
            try {
                return String(file.contentsToByteArray())
            } catch (e: IOException) {
                TestCase.fail("Unable to read file contents")
                return null
            }

        }

    @Throws(Exception::class)
    public override fun setUp() {
        super.setUp()

        val project = project
        val file = getVirtualFile(project, filePath)
        originalContents = file.contentsToByteArray()
        psiOriginalContents = psiFile.text.toByteArray()
    }

    @Throws(Exception::class)
    public override fun tearDown() {
        super.tearDown()

        if (virtualFileContentsChanged())
            restoreFile()
    }

    /*
     * Methods
     */

    fun makeTextDocumentItem(version: Int = 0) =
        TextDocumentItem(file.url, "java", version, currentFileContentsSafely!!)

    fun makeTextDocumentItem(filePath: String, version: Int = 0): TextDocumentItem {
        val file = getPsiFile(project, filePath) ?: throw FileNotFoundException(filePath)
        return TextDocumentItem(file.virtualFile.url, "java", version, file.text)
    }

    fun makeVersionedTextDocumentIdentifier(version: Int) =
        VersionedTextDocumentIdentifier(version).apply{ uri = file.url }

    fun makeVersionedTextDocumentIdentifier(filePath: String, version: Int): VersionedTextDocumentIdentifier {
        val file = getVirtualFile(project, filePath)
        return VersionedTextDocumentIdentifier(version).apply { uri = file.url }
    }

    fun contentsChanged(original: ByteArray, updatedContents: ByteArray): Boolean {
        try {
            val len = updatedContents.size
            if (len != original.size) {
                // definitely not same
                LOG.debug("Length differed, orig: ${original.size}, now: $len")
                return true
            } else {
                // is it the same?
                for (i in 0 until len) {
                    if (original[i] != updatedContents[i]) {
                        LOG.debug("Byte at $i differed, orig: ${original[i]}, now: ${updatedContents[i]}")
                        return true
                    }
                }
            }
        } catch (e: IOException) {
            TestCase.fail("Unable to read file contents: " + e.message)
        }

        // no change
        return false
    }

    fun virtualFileContentsChanged() = contentsChanged(originalContents!!, file.contentsToByteArray())
    fun psiContentsChanged() = contentsChanged(psiOriginalContents!!, psiFile.text.toByteArray())

    /*
     * Assertions
     */

    protected fun assertVirtualFileContentsChanged() {
       assertTrue("Expected file contents to have changed", virtualFileContentsChanged())
    }

    protected fun assertVirtualFileContentsUnchanged() {
        if (!virtualFileContentsChanged())
            return

        assertSame(String(originalContents!!), currentFileContentsSafely)
    }

    protected fun assertPsiContentsChanged() {
        assertTrue("Expected PSI contents to have changed", psiContentsChanged())
    }

    protected fun assertPsiContentsUnchanged() {
        assertFalse("Expected PSI contents to be unchanged", psiContentsChanged())
    }

    protected fun assertVirtualFileDoesNotContain(expectedContents: CharSequence) {
        val actualContents = currentFileContentsSafely
        assertFalse(actualContents?.contains(expectedContents)!!)
    }

    protected fun assertVirtualFileContains(expectedContents: CharSequence) {
        val actualContents = currentFileContentsSafely
        assert(actualContents?.contains(expectedContents)!!)
    }

    /**
     * Convenience to assert both that the contents have changed somehow,
     * and that they've changed in the way you expect---by "now" containing
     * something they (presumably) didn't before
     */
    protected fun assertVirtualFileNowContains(expectedContents: CharSequence) {
        assertVirtualFileContentsChanged()
        assertVirtualFileContains(expectedContents)
    }


    /*
     * utils
     */

    /** For testing  */
    protected fun dumpFileContents() {
        println(currentFileContentsSafely)
    }

    @Throws(IOException::class)
    private fun restoreFile() {
        val file = file

        // NB: At this point, the file on disk is correct,
        //  but the PsiFile is still referencing the modified stuff

        val project = project
        val originalString = String(originalContents!!)

        val psi = ApplicationManager.getApplication().runWriteAction(
            ThrowableComputable<PsiFile, IOException> {
                file.setBinaryContent(originalContents!!)

                val psi = getPsiFile(project, file)!!
                val doc = getDocument(psi) ?: throw IOException("No document found!")

                FileDocumentManager.getInstance().reloadFromDisk(doc)

                // reloadFromDisk() seems to convert CRLF to LF, but loading the byte array from the VirtualFile
                // correctly gives the CRLF as on disk.
                assertEquals(originalString.replace("\r\n", "\n"), doc.text)

                // now, commit changes so the PsiFile is updated
                PsiDocumentManager.getInstance(project).commitDocument(doc)
                psi
            })

        // Same issue as above.
        assertEquals(psi.text, originalString.replace("\r\n", "\n"))
    }
}
