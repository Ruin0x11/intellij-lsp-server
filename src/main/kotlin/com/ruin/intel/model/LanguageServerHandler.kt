package com.ruin.intel.model

import com.googlecode.jsonrpc4j.JsonRpcMethod
import com.googlecode.jsonrpc4j.JsonRpcParam
import com.ruin.intel.values.*

/**
 * Interface which implements methods from the Language Server Protocol.
 */
interface LanguageServerHandler {
    @JsonRpcMethod("initialize")
    fun onInitialize(@JsonRpcParam(value="processId") processId: Int,
                     @JsonRpcParam(value="rootUri") rootUri: DocumentUri,
                     @JsonRpcParam(value="capabilities") capabilities: ClientCapabilities) : InitializeResult

    @JsonRpcMethod("textDocument/completion")
    fun onTextDocumentCompletion(
            @JsonRpcParam(value="textDocument") textDocumentIdentifier: TextDocumentIdentifier,
            @JsonRpcParam(value="position") position: Position,
            @JsonRpcParam(value="triggerKind") triggerKind: Int,
            @JsonRpcParam(value="triggerCharacter") triggerCharacter: String?) : List<CompletionItem>

    @JsonRpcMethod("textDocument/hover")
    fun onTextDocumentHover(
        @JsonRpcParam(value="textDocument") textDocumentIdentifier: TextDocumentIdentifier,
        @JsonRpcParam(value="position") position: Position) : Hover?

    @JsonRpcMethod("shutdown")
    fun onShutdown()

    @JsonRpcMethod("exit")
    fun onExit()


    @JsonRpcMethod("initialized")
    fun onNotifyInitialized()

    @JsonRpcMethod("\$/cancelRequest")
    fun onNotifyCancelRequest(
        @JsonRpcParam(value="params") params: CancelParams)

    @JsonRpcMethod("textDocument/didOpen")
    fun onNotifyTextDocumentDidOpen(
        @JsonRpcParam(value="textDocument") textDocument: TextDocumentItem)

    @JsonRpcMethod("textDocument/didClose")
    fun onNotifyTextDocumentDidClose(
        @JsonRpcParam(value="textDocument") textDocument: TextDocumentIdentifier)

    @JsonRpcMethod("textDocument/didChange")
    fun onNotifyTextDocumentDidChange(
        @JsonRpcParam(value="textDocument") textDocument: VersionedTextDocumentIdentifier,
        @JsonRpcParam(value="contentChanges") contentChanges:List<TextDocumentContentChangeEvent>)
}
