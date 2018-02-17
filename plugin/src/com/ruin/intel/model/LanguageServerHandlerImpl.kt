package com.ruin.intel.model

import com.ruin.intel.values.CompletionItem

class LanguageServerHandlerImpl : LanguageServerHandler {
    override fun doCompletion(triggerKind: Int, triggerCharacter: String?): List<CompletionItem> {
        return listOf(CompletionItem("yuh"),
                CompletionItem("working"),
                CompletionItem("sort of")
        )
    }
}