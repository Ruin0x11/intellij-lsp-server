package com.ruin.intel.model

import com.googlecode.jsonrpc4j.ErrorResolver
import com.intellij.openapi.components.ServiceManager
import com.intellij.openapi.diagnostic.Logger
import com.ruin.intel.commands.completion.CompletionCommand
import com.ruin.intel.commands.definition.DefinitionCommand
import com.ruin.intel.commands.hover.HoverCommand
import com.ruin.intel.values.*

fun defaultServerCapabilities() : ServerCapabilities {
    return ServerCapabilities(textDocumentSync = null,
            hoverProvider = null,
            completionProvider = CompletionOptions(false, listOf(".", "@", "#")),
            signatureHelpProvider = null,
            definitionProvider = true,
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

    override fun onShutdown() {
        checkInitialized()
    }

    override fun onExit() {
        checkInitialized()
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

    override fun onTextDocumentDefinition(textDocumentIdentifier: TextDocumentIdentifier, position: Position): Location {
        checkInitialized()

        val result = DefinitionCommand(textDocumentIdentifier, position).execute()

        return result.fold({ value -> value
        }, { error ->
            throw error
        })
    }

    override fun onTextDocumentCompletion(textDocumentIdentifier: TextDocumentIdentifier,
                                          position: Position,
                                          triggerKind: Int,
                                          triggerCharacter: String?): CompletionList {
        checkInitialized()

        val result = CompletionCommand(textDocumentIdentifier, position, triggerKind, triggerCharacter).execute()

        return result.fold({ value -> value
        }, { error ->
            throw error
        })
    }

    override fun onNotifyInitialized() {
        context.wasInitialized = true
    }

    override fun onNotifyCancelRequest(id: String) {
        checkInitialized()
    }

    override fun onNotifyTextDocumentDidOpen(textDocument: TextDocumentItem) {
        checkInitialized()
        workspace().onTextDocumentOpened(textDocument)
    }

    override fun onNotifyTextDocumentDidClose(textDocument: VersionedTextDocumentIdentifier) {
        checkInitialized()
        workspace().onTextDocumentClosed(textDocument)
    }

    override fun onNotifyTextDocumentDidChange(textDocument: VersionedTextDocumentIdentifier, contentChanges: List<TextDocumentContentChangeEvent>) {
        checkInitialized()
        workspace().onTextDocumentChanged(textDocument, contentChanges)
    }

    override fun onNotifyTextDocumentDidSave(textDocument: VersionedTextDocumentIdentifier, text: String?) {
        checkInitialized()
        workspace().onTextDocumentSaved(textDocument, text)
    }

    fun checkInitialized() {
        if (!initialized()) {
            throw LanguageServerException("Server was not initialized.")
        }
    }
    fun initialized() = context.wasInitialized
}
