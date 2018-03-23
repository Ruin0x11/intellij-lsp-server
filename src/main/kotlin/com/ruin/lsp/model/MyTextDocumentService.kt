package com.ruin.lsp.model

import com.intellij.openapi.components.ServiceManager
import com.ruin.lsp.commands.document.action.CodeActionCommand
import com.ruin.lsp.commands.document.completion.CompletionCommand
import com.ruin.lsp.commands.document.completion.CompletionItemResolveCommand
import com.ruin.lsp.commands.document.find.FindDefinitionCommand
import com.ruin.lsp.commands.document.find.FindUsagesCommand
import com.ruin.lsp.commands.document.formatting.DocumentFormattingCommand
import com.ruin.lsp.commands.document.highlight.DocumentHighlightCommand
import com.ruin.lsp.commands.document.hover.HoverCommand
import com.ruin.lsp.commands.document.symbol.DocumentSymbolCommand
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.services.TextDocumentService
import java.util.concurrent.CompletableFuture

class MyTextDocumentService(val server: MyLanguageServer) : TextDocumentService {
    val workspace: WorkspaceManager by lazy { ServiceManager.getService<WorkspaceManager>(WorkspaceManager::class.java)!! }

    override fun resolveCompletionItem(unresolved: CompletionItem): CompletableFuture<CompletionItem> {
        val cache = PreviousCompletionCacheService.getInstance()
        val uri = cache.lastUri()
        val command = CompletionItemResolveCommand(unresolved)
        return asInvokeAndWaitFuture(uri, command, server.client, server)
    }

    override fun codeAction(params: CodeActionParams): CompletableFuture<MutableList<out Command>> =
        asInvokeAndWaitFuture(params.textDocument.uri, CodeActionCommand(params.range, params.context))

    override fun hover(position: TextDocumentPositionParams): CompletableFuture<Hover> =
        asInvokeAndWaitFuture(position.textDocument.uri, HoverCommand(position.position), server.client)

    override fun documentHighlight(position: TextDocumentPositionParams): CompletableFuture<MutableList<out DocumentHighlight>> =
        asInvokeAndWaitFuture(position.textDocument.uri, DocumentHighlightCommand(position.position), server.client)

    override fun definition(position: TextDocumentPositionParams): CompletableFuture<MutableList<out Location>> =
        asInvokeAndWaitFuture(position.textDocument.uri, FindDefinitionCommand(position.position), server.client)

    override fun formatting(params: DocumentFormattingParams): CompletableFuture<MutableList<out TextEdit>> =
        asInvokeAndWaitFuture(params.textDocument.uri, DocumentFormattingCommand(params.options))

    override fun rangeFormatting(params: DocumentRangeFormattingParams): CompletableFuture<MutableList<out TextEdit>> =
        asInvokeAndWaitFuture(params.textDocument.uri, DocumentFormattingCommand(params.options, params.range))

    override fun onTypeFormatting(params: DocumentOnTypeFormattingParams): CompletableFuture<MutableList<out TextEdit>> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun codeLens(params: CodeLensParams): CompletableFuture<MutableList<out CodeLens>> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun rename(params: RenameParams): CompletableFuture<WorkspaceEdit> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun completion(position: TextDocumentPositionParams): CompletableFuture<Either<MutableList<CompletionItem>, CompletionList>> =
        asCancellableInvokeAndWaitFuture(position.textDocument.uri, CompletionCommand(position.position,
            server.context.clientCapabilities?.textDocument?.completion?.completionItem?.snippetSupport ?: false), server.client)

    override fun documentSymbol(params: DocumentSymbolParams): CompletableFuture<MutableList<out SymbolInformation>> =
        asCancellableInvokeAndWaitFuture(params.textDocument.uri, DocumentSymbolCommand(params.textDocument), server.client)

    override fun signatureHelp(position: TextDocumentPositionParams): CompletableFuture<SignatureHelp> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun references(params: ReferenceParams): CompletableFuture<MutableList<out Location>> =
        asCancellableInvokeAndWaitFuture(params.textDocument.uri, FindUsagesCommand(params.position), server.client)

    override fun resolveCodeLens(unresolved: CodeLens): CompletableFuture<CodeLens> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun didOpen(params: DidOpenTextDocumentParams) {
        workspace.onTextDocumentOpened(params, server.client, server)
        server.computeDiagnostics(params.textDocument.uri)
    }

    override fun didChange(params: DidChangeTextDocumentParams) {
        workspace.onTextDocumentChanged(params)
        server.computeDiagnostics(params.textDocument.uri)
    }

    override fun didSave(params: DidSaveTextDocumentParams) {
        workspace.onTextDocumentSaved(params)
        server.computeDiagnostics(params.textDocument.uri)
    }

    override fun didClose(params: DidCloseTextDocumentParams) {
        workspace.onTextDocumentClosed(params)
    }
}
