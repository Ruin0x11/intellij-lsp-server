package com.ruin.intel.model

import com.googlecode.jsonrpc4j.JsonRpcMethod
import com.googlecode.jsonrpc4j.JsonRpcParam
import com.ruin.intel.values.*

/**
 * Interface which implements methods makeCompletionParameters the Language Server Protocol.
 */
interface LanguageServerHandler {
    @JsonRpcMethod("initialize")
    fun onInitialize(@JsonRpcParam(value="processId") processId: Int,
                     @JsonRpcParam(value="rootUri") rootUri: DocumentUri,
                     @JsonRpcParam(value="capabilities") capabilities: ClientCapabilities) : InitializeResult

    @JsonRpcMethod("shutdown")
    fun onShutdown()

    @JsonRpcMethod("exit")
    fun onExit()


    @JsonRpcMethod("textDocument/find")
    fun onTextDocumentDefinition(
        @JsonRpcParam(value="textDocument") textDocumentIdentifier: TextDocumentIdentifier,
        @JsonRpcParam(value="position") position: Position): List<Location>

    @JsonRpcMethod("textDocument/implementation")
    fun onTextDocumentImplementation(
        @JsonRpcParam(value="textDocument") textDocumentIdentifier: TextDocumentIdentifier,
        @JsonRpcParam(value="position") position: Position): List<Location>

    @JsonRpcMethod("textDocument/completion")
    fun onTextDocumentCompletion(
            @JsonRpcParam(value="textDocument") textDocumentIdentifier: TextDocumentIdentifier,
            @JsonRpcParam(value="position") position: Position,
            @JsonRpcParam(value="triggerKind") triggerKind: Int,
            @JsonRpcParam(value="triggerCharacter") triggerCharacter: String?) : CompletionList

    @JsonRpcMethod("textDocument/hover")
    fun onTextDocumentHover(
        @JsonRpcParam(value="textDocument") textDocumentIdentifier: TextDocumentIdentifier,
        @JsonRpcParam(value="position") position: Position) : Hover?


    @JsonRpcMethod("initialized")
    fun onNotifyInitialized()

    @JsonRpcMethod("\$/cancelRequest")
    fun onNotifyCancelRequest(
        @JsonRpcParam(value="id") id: String)

    @JsonRpcMethod("textDocument/didOpen")
    fun onNotifyTextDocumentDidOpen(
        @JsonRpcParam(value="textDocument") textDocument: TextDocumentItem)

    @JsonRpcMethod("textDocument/didClose")
    fun onNotifyTextDocumentDidClose(
        // lsp-mode sends VersionedTextDocumentIdentifier instead
        //@JsonRpcParam(value="textDocument") textDocument: TextDocumentIdentifier,
        @JsonRpcParam(value="textDocument") textDocument: VersionedTextDocumentIdentifier
    )

    @JsonRpcMethod("textDocument/didChange")
    fun onNotifyTextDocumentDidChange(
        @JsonRpcParam(value="textDocument") textDocument: VersionedTextDocumentIdentifier,
        @JsonRpcParam(value="contentChanges") contentChanges: List<TextDocumentContentChangeEvent>)

    @JsonRpcMethod("textDocument/didSave")
    fun onNotifyTextDocumentDidSave(
        // lsp-mode sends VersionedTextDocumentIdentifier instead
        //@JsonRpcParam(value="textDocument") textDocument: TextDocumentIdentifier,
        @JsonRpcParam(value="textDocument") textDocument: VersionedTextDocumentIdentifier,
        @JsonRpcParam(value="insertText") text: String?)
}
