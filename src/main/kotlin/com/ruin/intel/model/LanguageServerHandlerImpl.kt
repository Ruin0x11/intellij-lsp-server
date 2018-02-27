package com.ruin.intel.model

import com.googlecode.jsonrpc4j.ErrorResolver
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.ServiceManager
import com.intellij.openapi.diagnostic.Logger
import com.ruin.intel.commands.CompletionCommand
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

    override fun doInitialize(processId: Int, rootUri: DocumentUri, capabilities: ClientCapabilities) : InitializeResult {
        context.wasInitialized = true
        LOG.info("INIT LSP")
        return InitializeResult(defaultServerCapabilities())
    }

    override fun doCompletion(textDocumentIdentifier: TextDocumentIdentifier,
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

    override fun doShutdown() {
        checkInitialized()
    }

    override fun doExit() {
        checkInitialized()
    }

    override fun notifyInitialized() {
        checkInitialized()
    }

    override fun notifyDidOpen(textDocument: TextDocumentItem) {
        checkInitialized()

        ApplicationManager.getApplication().invokeAndWait {
            val ws = workspace()
            ws.getExistingPsiFile(textDocument.uri)
        }
    }

    override fun notifyDidClose(textDocument: TextDocumentIdentifier) {
        checkInitialized()

        //val ws = workspace()
        //ws.closePsiFile(textDocument.uri)
    }

    override fun notifyDidChange(textDocument: VersionedTextDocumentIdentifier, contentChanges: List<TextDocumentContentChangeEvent>) {
        //val ws = workspace()
        //val changedFile = ws.getExistingPsiFile(textDocument.uri)
        //contentChanges.forEach {
        //    // applyChangeToPsi(changedFile, it)
        //}
    }

    fun checkInitialized() {
        if (!initialized()) {
            throw LanguageServerException(ErrorResolver.JsonError.BULK_ERROR)
        }
    }
    fun initialized() = context.wasInitialized
}
