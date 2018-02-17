package com.ruin.intel.model

import com.googlecode.jsonrpc4j.JsonRpcMethod
import com.googlecode.jsonrpc4j.JsonRpcParam
import com.ruin.intel.values.ClientCapabilities
import com.ruin.intel.values.CompletionItem

/**
 * Interface which implements methods from the Language Server Protocol.
 */
interface LanguageServerHandler {
    @JsonRpcMethod("initialize")
    fun doInitialize(@JsonRpcParam(value="processId") processId: Int,
                     @JsonRpcParam(value="rootUri") rootUri: String,
                     @JsonRpcParam(value="capabilities") capabilities: ClientCapabilities,
                     @JsonRpcParam(value="trace") trace: String?)

    @JsonRpcMethod("textDocument/completion")
    fun doCompletion(@JsonRpcParam(value="triggerKind") triggerKind: Int,
                     @JsonRpcParam(value="triggerCharacter") triggerCharacter: String?) : List<CompletionItem>

    @JsonRpcMethod("shutdown")
    fun doShutdown()

    @JsonRpcMethod("exit")
    fun doExit()


    @JsonRpcMethod("initialized")
    fun notifyInitialized()
}