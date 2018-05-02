package com.ruin.lsp.util

import com.intellij.codeInsight.completion.OffsetMap
import com.intellij.codeInsight.completion.OffsetsInFile
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.impl.DocumentImpl
import com.intellij.openapi.util.Key
import com.intellij.openapi.util.Pair
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiFile
import com.intellij.reference.SoftReference


private val LOG = Logger.getInstance("#com.ruin.lsp.util.FileUtil")
private val FILE_COPY_KEY = Key.create<SoftReference<Pair<PsiFile, Document>>>("CompletionFileCopy")

/**
 * Duplicate a PsiFile; from CodeCompletionHandlerBase
 * @param file file to copy
 * @return the copied file
 */
fun obtainFileCopy(file: PsiFile): PsiFile {
    val virtualFile = file.virtualFile
    val mayCacheCopy = file.isPhysical &&
        // we don't want to cache code fragment copies even if they appear to be physical
        virtualFile != null && virtualFile.isInLocalFileSystem
    if (mayCacheCopy) {
        val cached = SoftReference.dereference(file.getUserData<SoftReference<Pair<PsiFile, Document>>>(FILE_COPY_KEY))
        if (cached != null && isCopyUpToDate(cached.second, cached.first, file)) {
            val copy = cached.first
            assertCorrectOriginalFile("Cached", file, copy)
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
        file.putUserData<SoftReference<Pair<PsiFile, Document>>>(FILE_COPY_KEY, SoftReference(Pair.create(copy, document)))
    }
    return copy
}

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


fun OffsetsInFile.toFileCopy(copyFile: PsiFile): OffsetsInFile {
    assertCorrectOriginalFile("Given ", file, copyFile)
    assert(copyFile.viewProvider.document!!.textLength == file.viewProvider.document!!.textLength)
    return mapOffsets(copyFile) { it }
}

private fun OffsetsInFile.mapOffsets(newFile: PsiFile, offsetFun: (Int) -> Int): OffsetsInFile {
    val map = OffsetMap(newFile.viewProvider.document!!)
    for (key in offsets.allOffsets) {
        map.addOffset(key, offsetFun(offsets.getOffset(key)))
    }
    return OffsetsInFile(newFile, map)
}
