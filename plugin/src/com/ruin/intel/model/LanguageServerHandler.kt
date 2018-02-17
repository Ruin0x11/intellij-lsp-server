package com.ruin.intel.model

import com.googlecode.jsonrpc4j.JsonRpcMethod
import com.ruin.intel.values.CompletionItem

interface LanguageServerHandler {
    @JsonRpcMethod("textDocument/completion")
    fun doCompletion(triggerKind: Int, triggerCharacter: String?) : List<CompletionItem>
}