package com.ruin.lsp.model

import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.openapi.components.ServiceManager
import java.util.concurrent.atomic.AtomicLong

class PreviousCompletionCacheService {
    private val completionId: AtomicLong = AtomicLong(0)
    private var elements: List<LookupElement> = listOf()

    fun incrementId() = completionId.incrementAndGet()

    fun cacheCompletion(elts: List<LookupElement>) {
        elements = elts
    }

    fun resolveItem(idx: CompletionResolveIndex): LookupElement? {
        if (idx.completionId != completionId.get()) {
            return null
        }

        return elements[idx.idx]
    }

    companion object {
        fun getInstance() =
            ServiceManager.getService<PreviousCompletionCacheService>(PreviousCompletionCacheService::class.java)!!
    }
}

data class CompletionResolveIndex(val completionId: Long, val idx: Int)
