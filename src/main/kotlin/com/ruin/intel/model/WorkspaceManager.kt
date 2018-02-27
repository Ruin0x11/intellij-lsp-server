package com.ruin.intel.model

import com.intellij.openapi.diagnostic.Logger
import com.intellij.psi.PsiFile
import com.ruin.intel.Util.resolvePsiFromUri
import com.ruin.intel.values.DocumentUri
import java.io.FileNotFoundException

class WorkspaceManager {
    val LOG = Logger.getInstance(WorkspaceManager::class.java)
    private val openPsiFiles: HashMap<DocumentUri, PsiFile> = HashMap()

    /**
     * Retrieves the PSI file for a file that exists on disk.
     * @param uri URI of the file
     * @throws FileNotFoundException if the file doesn't exist
     */
    fun getExistingPsiFile(uri: DocumentUri): PsiFile {
        val file = openPsiFiles.getOrPut(uri, {
            val pair = resolvePsiFromUri(uri) ?: throw FileNotFoundException(uri)
            pair.second
        })
        assert(file.isValid)
        assert(file.project.isOpen)
        LOG.info("Opened file at $uri")
        return file
    }

    /**
     * Removes the reference of the PSI file corresponding to the given URI from this workspace.
     * @param uri URI of the file
     */
    fun closePsiFile(uri: DocumentUri) {
        assert(openPsiFiles.containsKey(uri))
        assert(openPsiFiles[uri]!!.isValid)
        openPsiFiles.remove(uri)
        LOG.info("Closed file at $uri")
    }

    ///**
    // * Retrieves the PSI file for a file on disk, or creates a new one if it doesn't exist.
    // * @param uri URI of the file
    // */
    //fun getOrOpenPsiFile(uri: DocumentUri): PsiFile {
    //    if(!openPsiFiles.containsKey(uri)) {
    //
    //    }
    //}
}
