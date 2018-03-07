package com.ruin.lsp.util

import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Document
import com.intellij.openapi.util.Key
import com.intellij.openapi.util.Trinity
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiFile
import com.intellij.psi.impl.PsiModificationTrackerImpl
import com.intellij.reference.SoftReference

private val LOG = Logger.getInstance("#com.ruin.lsp.util.FileUtil")
private val FILE_COPY_KEY: Key<SoftReference<Trinity<PsiFile, Document, Long>>> = Key.create("CompletionFileCopy")

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
        val cached = SoftReference.dereference(file.getUserData(FILE_COPY_KEY))
        if (cached != null && cached.first.javaClass == file.javaClass && isCopyUpToDate(cached.second, cached.first)) {
            val copy = cached.first
            if (copy.viewProvider.modificationStamp > file.viewProvider.modificationStamp && cached.third != combinedOffsets) {
                // the copy PSI might have some caches that are not cleared on its modification because there are no events in the copy
                //   so, clear all the caches
                // hopefully it's a rare situation that the user invokes completion in different parts of the file
                //   without modifying anything physical in between
                (file.manager.modificationTracker as PsiModificationTrackerImpl).incCounter()
            }
            val document = cached.second!!
            file.putUserData(FILE_COPY_KEY, SoftReference(Trinity.create(copy, document, combinedOffsets)))

            val originalDocument = file.viewProvider.document!!
            assert(originalDocument.textLength == file.textLength) { originalDocument }
            document.setText(originalDocument.immutableCharSequence)
            return copy
        }
    }

    val copy = file.copy() as PsiFile
    if (mayCacheCopy) {
        val document = copy.viewProvider.document!!
        file.putUserData(FILE_COPY_KEY, SoftReference(Trinity.create(copy, document, combinedOffsets)))
    }
    return copy
}

/** Also from CodeCompletionHandlerBase  */
private fun isCopyUpToDate(document: Document, file: PsiFile): Boolean {
    if (!file.isValid) {
        return false
    }
    // the elt file cache might have been cleared by some external activity,
    // in which case PSI-document sync may stop working
    val current = PsiDocumentManager.getInstance(file.project).getPsiFile(document)
    return current != null && current.viewProvider.getPsi(file.language) === file
}
