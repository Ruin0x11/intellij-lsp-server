package com.ruin.lsp.model

import com.google.gson.JsonObject
import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.openapi.components.ServiceManager
import com.intellij.psi.PsiFile
import com.ruin.lsp.util.getURIForFile
import com.ruin.lsp.values.DocumentUri
import java.util.concurrent.atomic.AtomicLong

/**
 * Stores the previous completion result, for later retrieval in the Completion Resolve request.
 */
class PreviousCompletionCacheService {
    private val myCompletionId: AtomicLong = AtomicLong(0)
    private var elements: List<LookupElement> = listOf()
    private var lastUri: DocumentUri = ""

    fun incrementId() = myCompletionId.incrementAndGet()

    fun cacheCompletion(file: PsiFile, elts: List<LookupElement>) {
        elements = elts
        lastUri = getURIForFile(file)
    }

    fun resolveItem(obj: JsonObject): LookupElement? {
        val completionId = obj.get("completionId") ?: return null
        val idx = obj.get("idx") ?: return null

        return resolveItem(CompletionResolveIndex(completionId.asLong, idx.asInt))
    }

    fun resolveItem(idx: CompletionResolveIndex): LookupElement? {
        if (idx.completionId != myCompletionId.get()) {
            return null
        }

        return elements[idx.idx]
    }

    fun lastUri(): DocumentUri = lastUri

    companion object {
        fun getInstance() =
            ServiceManager.getService<PreviousCompletionCacheService>(PreviousCompletionCacheService::class.java)!!
    }
}

data class CompletionResolveIndex(val completionId: Long, val idx: Int)
