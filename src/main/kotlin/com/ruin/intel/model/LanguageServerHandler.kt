package com.ruin.intel.model

import com.googlecode.jsonrpc4j.JsonRpcMethod
import com.googlecode.jsonrpc4j.JsonRpcParam
import com.ruin.intel.values.*

/**
 * Interface which implements methods from the Language Server Protocol.
 */
interface LanguageServerHandler {
    @JsonRpcMethod("initialize")
    fun doInitialize(@JsonRpcParam(value="processId") processId: Int,
                     @JsonRpcParam(value="rootUri") rootUri: DocumentUri,
                     @JsonRpcParam(value="capabilities") capabilities: ClientCapabilities) : InitializeResult

    @JsonRpcMethod("textDocument/completion")
    fun doCompletion(
            @JsonRpcParam(value="textDocument") textDocumentIdentifier: TextDocumentIdentifier,
            @JsonRpcParam(value="position") position: Position,
            @JsonRpcParam(value="triggerKind") triggerKind: Int,
            @JsonRpcParam(value="triggerCharacter") triggerCharacter: String?) : List<CompletionItem>

    @JsonRpcMethod("shutdown")
    fun doShutdown()

    @JsonRpcMethod("exit")
    fun doExit()


    @JsonRpcMethod("initialized")
    fun notifyInitialized()

    @JsonRpcMethod("textDocument/didOpen")
    fun notifyDidOpen(
        @JsonRpcParam(value="textDocument") textDocument: TextDocumentItem
    )

    @JsonRpcMethod("textDocument/didClose")
    fun notifyDidClose(
        @JsonRpcParam(value="textDocument") textDocument: TextDocumentIdentifier
    )

    @JsonRpcMethod("textDocument/didChange")
    fun notifyDidChange(
        @JsonRpcParam(value="textDocument") textDocument: VersionedTextDocumentIdentifier,
        @JsonRpcParam(value="contentChanges") contentChanges:List<TextDocumentContentChangeEvent>
    )
}
