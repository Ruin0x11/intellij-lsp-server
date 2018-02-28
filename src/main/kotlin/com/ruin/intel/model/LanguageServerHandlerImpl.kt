package com.ruin.intel.model

import com.googlecode.jsonrpc4j.ErrorResolver
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.ServiceManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.util.Computable
import com.ruin.intel.commands.CompletionCommand
import com.ruin.intel.commands.HoverCommand
import com.ruin.intel.values.*

fun defaultServerCapabilities() : ServerCapabilities {
    return ServerCapabilities(textDocumentSync = null,
            hoverProvider = null,
            completionProvider = null,
            signatureHelpProvider = null,
            definitionProvider = false,
            referencesProvider = false,
            documentHighlightProvider = false,
            documentSymbolProvider = false,
            workspaceSymbolProvider = false,
            codeActionProvider = false,
            codeLensProvider = null,
            documentFormattingProvider = false,
            documentRangeFormattingProvider = false,
            documentOnTypeFormattingProvider = null,
            renameProvider = false,
            documentLinkProvider = null,
            executeCommandProvider = null,
            experimental = null)
}

fun workspace() = ServiceManager.getService<WorkspaceManager>(WorkspaceManager::class.java)!!

class LanguageServerHandlerImpl(val context: Context) : LanguageServerHandler {
    val LOG = Logger.getInstance(LanguageServerHandlerImpl::class.java)

    override fun onInitialize(processId: Int, rootUri: DocumentUri, capabilities: ClientCapabilities) : InitializeResult {
        context.wasInitialized = true
        LOG.info("INIT LSP")
        return InitializeResult(defaultServerCapabilities())
    }

    override fun onTextDocumentHover(textDocumentIdentifier: TextDocumentIdentifier, position: Position): Hover? {
        checkInitialized()
        var result = HoverCommand(textDocumentIdentifier, position).execute()

        return result.fold({ value -> if (value.isEmpty())
            null
        else
            Hover(value, null)
        }, { error ->
            throw error
        })
    }

    override fun onTextDocumentCompletion(textDocumentIdentifier: TextDocumentIdentifier,
                                          position: Position,
                                          triggerKind: Int,
                                          triggerCharacter: String?): List<CompletionItem> {
        checkInitialized()

        val result = CompletionCommand(textDocumentIdentifier, position, triggerKind, triggerCharacter).execute()

        return result.fold({ value -> value
        }, { error ->
            throw error
        })
    }

    override fun onShutdown() {
        checkInitialized()
    }

    override fun onExit() {
        checkInitialized()
    }

    override fun onNotifyInitialized() {
        context.wasInitialized = true
    }

    override fun onNotifyCancelRequest(params: CancelParams) {
        checkInitialized()
    }

    override fun onNotifyTextDocumentDidOpen(textDocument: TextDocumentItem) {
        checkInitialized()
        workspace().onTextDocumentOpened(textDocument)
    }

    override fun onNotifyTextDocumentDidClose(textDocument: TextDocumentIdentifier) {
        checkInitialized()
        workspace().onTextDocumentClosed(textDocument)
    }

    override fun onNotifyTextDocumentDidChange(textDocument: VersionedTextDocumentIdentifier, contentChanges: List<TextDocumentContentChangeEvent>) {
        checkInitialized()
        workspace().onTextDocumentChanged(textDocument, contentChanges)
    }

    override fun onNotifyTextDocumentDidSave(textDocument: TextDocumentIdentifier, text: String?) {
        checkInitialized()
        workspace().onTextDocumentSaved(textDocument, text)
    }

    fun checkInitialized() {
        if (!initialized()) {
            throw LanguageServerException(ErrorResolver.JsonError.BULK_ERROR)
        }
    }
    fun initialized() = context.wasInitialized
}
