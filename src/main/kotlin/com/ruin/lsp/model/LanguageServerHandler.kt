package com.ruin.lsp.model

import com.googlecode.jsonrpc4j.JsonRpcMethod
import com.googlecode.jsonrpc4j.JsonRpcParam
import com.ruin.lsp.values.*

/**
 * Interface which implements methods from the Language Server Protocol.
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


    @JsonRpcMethod("textDocument/completion")
    fun onTextDocumentCompletion(
        @JsonRpcParam(value="textDocument") textDocumentIdentifier: TextDocumentIdentifier,
        @JsonRpcParam(value="position") position: Position,
        @JsonRpcParam(value="triggerKind") triggerKind: Int,
        @JsonRpcParam(value="triggerCharacter") triggerCharacter: String?) : CompletionList

    @JsonRpcMethod("textDocument/definition")
    fun onTextDocumentDefinition(
        @JsonRpcParam(value="textDocument") textDocumentIdentifier: TextDocumentIdentifier,
        @JsonRpcParam(value="position") position: Position): List<Location>

    @JsonRpcMethod("textDocument/implementation")
    fun onTextDocumentImplementation(
        @JsonRpcParam(value="textDocument") textDocumentIdentifier: TextDocumentIdentifier,
        @JsonRpcParam(value="position") position: Position): List<Location>

    @JsonRpcMethod("textDocument/references")
    fun onTextDocumentReferences(
        @JsonRpcParam(value="textDocument") textDocumentIdentifier: TextDocumentIdentifier,
        @JsonRpcParam(value="position") position: Position): List<Location>

    @JsonRpcMethod("textDocument/documentHighlight")
    fun onTextDocumentDocumentHighlight(
        @JsonRpcParam(value="textDocument") textDocumentIdentifier: TextDocumentIdentifier,
        @JsonRpcParam(value="position") position: Position): List<DocumentHighlight>

    @JsonRpcMethod("textDocument/documentSymbol")
    fun onTextDocumentDocumentSymbol(
        @JsonRpcParam(value="textDocument") textDocumentIdentifier: TextDocumentIdentifier): List<SymbolInformation>


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
