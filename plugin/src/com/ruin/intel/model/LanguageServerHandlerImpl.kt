package com.ruin.intel.model

import com.googlecode.jsonrpc4j.ErrorResolver
import com.ruin.intel.values.ClientCapabilities
import com.ruin.intel.values.CompletionItem

class LanguageServerHandlerImpl(val parent: LanguageServerHttpServerHandler) : LanguageServerHandler {
    override fun doInitialize(processId: Int, rootUri: String, capabilities: ClientCapabilities, trace: String?) {
        parent.wasInitialized = true
    }

    override fun doCompletion(triggerKind: Int, triggerCharacter: String?): List<CompletionItem> {
        if (!initialized()) {
            throw LanguageServerException(ErrorResolver.JsonError.BULK_ERROR)
        }

        return listOf(CompletionItem("yuh"),
                CompletionItem("working"),
                CompletionItem("sort of")
        )
    }

    override fun doShutdown() {
        if (!initialized()) {
            throw LanguageServerException(ErrorResolver.JsonError.BULK_ERROR)
        }
    }

    override fun doExit() {
        if (!initialized()) {
            throw LanguageServerException(ErrorResolver.JsonError.BULK_ERROR)
        }
    }

    override fun notifyInitialized() {
        if (!initialized()) {
            throw LanguageServerException(ErrorResolver.JsonError.BULK_ERROR)
        }
    }


    fun initialized() = parent.wasInitialized

}