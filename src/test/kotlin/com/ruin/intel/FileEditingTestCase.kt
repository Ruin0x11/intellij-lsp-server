package org.intellivim

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.util.ThrowableComputable
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiFile
import com.ruin.intel.BaseTestCase
import com.ruin.intel.Util.getDocument
import com.ruin.intel.Util.getPsiFile
import com.ruin.intel.Util.getVirtualFile
import junit.framework.TestCase
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



    private// won't actually get here
    val currentFileContentsSafely: String?
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
    }

    @Throws(Exception::class)
    public override fun tearDown() {
        super.tearDown()

        if (fileContentsChanged())
            restoreFile()
    }

    /*
     * Methods
     */

    fun fileContentsChanged(): Boolean {
        try {
            val file = file
            val updatedContents = file.contentsToByteArray()
            val len = updatedContents.size
            if (len != originalContents!!.size) {
                // definitely not same
                return true
            } else {
                // is it the same?
                for (i in 0 until len) {
                    if (originalContents!![i] != updatedContents[i]) {
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

    /*
     * Assertions
     */

    protected fun assertFileContentsChanged() {
       assertTrue("Expected file contents to have changed", fileContentsChanged())
    }

    protected fun assertFileContentsUnchanged() {
        if (!fileContentsChanged())
            return

        assertSame(String(originalContents!!), currentFileContentsSafely)
    }

    protected fun assertFileDoesNotContain(expectedContents: CharSequence) {
        val actualContents = currentFileContentsSafely
        assertFalse(actualContents?.contains(expectedContents)!!)
    }

    protected fun assertFileContains(expectedContents: CharSequence) {
        val actualContents = currentFileContentsSafely
        assert(actualContents?.contains(expectedContents)!!)
    }

    /**
     * Convenience to assert both that the contents have changed somehow,
     * and that they've changed in the way you expect---by "now" containing
     * something they (presumably) didn't before
     */
    protected fun assertFileNowContains(expectedContents: CharSequence) {
        assertFileContentsChanged()
        assertFileContains(expectedContents)
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
                val doc = getDocument(psi)

                FileDocumentManager.getInstance().reloadFromDisk(doc)
                assertEquals(doc.getText(), originalString)

                // now, commit changes so the PsiFile is updated
                PsiDocumentManager.getInstance(project).commitDocument(doc)
                psi
            })

        assertEquals(psi.text, originalString)
    }
}
