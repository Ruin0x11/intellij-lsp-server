package com.ruin.lsp.util

import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.impl.DocumentImpl
import com.intellij.openapi.util.Key
import com.intellij.openapi.util.Pair
import com.intellij.openapi.util.Trinity
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiFile
import com.intellij.psi.impl.PsiModificationTrackerImpl
import com.intellij.reference.SoftReference
import com.intellij.openapi.util.Conditions.cached



private val LOG = Logger.getInstance("#com.ruin.lsp.util.FileUtil")
private val FILE_COPY_KEY = Key.create<SoftReference<Trinity<PsiFile, Document, Long>>>("CompletionFileCopy")

/**
 * Duplicate a PsiFile; from CodeCompletionHandlerBase
 * @param file file to copy
 * @param startOffset startOffset offset
 * @param endOffset startOffset end offset
 * @return the copied file
 */
fun createFileCopy(file: PsiFile, startOffset: Long, endOffset: Long): PsiFile {
    val virtualFile = file.virtualFile
    val mayCacheCopy = file.isPhysical &&
        // we don't want to cache code fragment copies even if they appear to be physical
        virtualFile != null && virtualFile.isInLocalFileSystem
    val combinedOffsets = startOffset + (endOffset shl 32)
    if (mayCacheCopy) {
        val cached = SoftReference.dereference(file.getUserData<SoftReference<Trinity<PsiFile, Document, Long>>>(FILE_COPY_KEY))
        if (cached != null && isCopyUpToDate(cached.second, cached.first, file)) {
            val copy = cached.first
            if (copy.viewProvider.modificationStamp > file.viewProvider.modificationStamp && cached.third != combinedOffsets) {
                // the copy PSI might have some caches that are not cleared on its modification because there are no events in the copy
                //   so, clear all the caches
                // hopefully it's a rare situation that the user invokes completion in different parts of the file
                //   without modifying anything physical in between
                (file.manager.modificationTracker as PsiModificationTrackerImpl).incCounter()
            }
            val document = cached.second
            assertCorrectOriginalFile("Cached", file, copy)
            val originalDocument = file.viewProvider.document!!
            assert(originalDocument.textLength == file.textLength) { originalDocument }
            document.replaceString(0, document.textLength, originalDocument.immutableCharSequence)
            return copy
        }
    }

    val copy = file.copy() as PsiFile
    if (copy.isPhysical || copy.viewProvider.isEventSystemEnabled) {
        LOG.error("File copy should be non-physical and non-event-system-enabled! Language=" + file.language + "; file=" + file + " of " + file.javaClass)
    }
    assertCorrectOriginalFile("New", file, copy)

    if (mayCacheCopy) {
        val document = copy.viewProvider.document!!
        syncAcceptSlashR(file.viewProvider.document, document)
        file.putUserData<SoftReference<Trinity<PsiFile, Document, Long>>>(FILE_COPY_KEY, SoftReference(Trinity.create(copy, document, combinedOffsets)))
    }
    return copy
}
fun createFileCopy(file: PsiFile) =
    createFileCopy(file, file.textRange.startOffset.toLong(), file.textRange.endOffset.toLong())

/** Also from CodeCompletionHandlerBase  */
private fun isCopyUpToDate(document: Document, copyFile: PsiFile, originalFile: PsiFile): Boolean {
    if (copyFile.javaClass != originalFile.javaClass ||
        !copyFile.isValid ||
        copyFile.name != originalFile.name) {
        return false
    }
    // the psi file cache might have been cleared by some external activity,
    // in which case PSI-document sync may stop working
    val current = PsiDocumentManager.getInstance(copyFile.project).getPsiFile(document)
    return current != null && current.viewProvider.getPsi(copyFile.language) === copyFile
}

/** Also from CodeCompletionHandlerBase  */
private fun syncAcceptSlashR(originalDocument: Document?, documentCopy: Document) {
    if (originalDocument !is DocumentImpl || documentCopy !is DocumentImpl) {
        return
    }

    documentCopy.setAcceptSlashR(originalDocument.acceptsSlashR())
}

/** From CompletionAssertions  */
private fun assertCorrectOriginalFile(prefix: String, file: PsiFile, copy: PsiFile) {
    if (copy.originalFile !== file) {
        throw AssertionError(prefix + " copied file doesn't have correct original: noOriginal=" + (copy.originalFile === copy) +
            "\n file " + fileInfo(file) +
            "\n copy " + fileInfo(copy))
    }
}

/** Also from CompletionAssertions  */
private fun fileInfo(file: PsiFile): String {
    return file.toString() + " of " + file.javaClass +
        " in " + file.viewProvider + ", languages=" + file.viewProvider.languages +
        ", physical=" + file.isPhysical
}
