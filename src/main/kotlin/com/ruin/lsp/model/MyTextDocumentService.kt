package com.ruin.lsp.model

import com.intellij.openapi.components.ServiceManager
import com.ruin.lsp.commands.completion.CompletionCommand
import com.ruin.lsp.commands.find.FindDefinitionCommand
import com.ruin.lsp.commands.find.FindUsagesCommand
import com.ruin.lsp.commands.highlight.DocumentHighlightCommand
import com.ruin.lsp.commands.hover.HoverCommand
import com.ruin.lsp.commands.symbol.DocumentSymbolCommand
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.services.TextDocumentService
import java.util.concurrent.CompletableFuture

class MyTextDocumentService(val context: MyLanguageServer) : TextDocumentService {
    val workspace: WorkspaceManager by lazy { ServiceManager.getService<WorkspaceManager>(WorkspaceManager::class.java)!! }

    override fun resolveCompletionItem(unresolved: CompletionItem): CompletableFuture<CompletionItem> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun codeAction(params: CodeActionParams): CompletableFuture<MutableList<out Command>> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun hover(position: TextDocumentPositionParams): CompletableFuture<Hover> =
        asInvokeAndWaitFuture(position.textDocument.uri, HoverCommand(position.position), context.client)

    override fun documentHighlight(position: TextDocumentPositionParams): CompletableFuture<MutableList<out DocumentHighlight>> =
        asInvokeAndWaitFuture(position.textDocument.uri, DocumentHighlightCommand(position.position), context.client)

    override fun onTypeFormatting(params: DocumentOnTypeFormattingParams): CompletableFuture<MutableList<out TextEdit>> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun definition(position: TextDocumentPositionParams): CompletableFuture<MutableList<out Location>> =
        asInvokeAndWaitFuture(position.textDocument.uri, FindDefinitionCommand(position.position), context.client)

    override fun rangeFormatting(params: DocumentRangeFormattingParams): CompletableFuture<MutableList<out TextEdit>> {
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
            context.context.clientCapabilities?.textDocument?.completion?.completionItem?.snippetSupport ?: false), context.client)

    override fun documentSymbol(params: DocumentSymbolParams): CompletableFuture<MutableList<out SymbolInformation>> =
        asCancellableInvokeAndWaitFuture(params.textDocument.uri, DocumentSymbolCommand(params.textDocument), context.client)

    override fun signatureHelp(position: TextDocumentPositionParams): CompletableFuture<SignatureHelp> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun formatting(params: DocumentFormattingParams): CompletableFuture<MutableList<out TextEdit>> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun references(params: ReferenceParams): CompletableFuture<MutableList<out Location>> =
        asCancellableInvokeAndWaitFuture(params.textDocument.uri, FindUsagesCommand(params.position), context.client)

    override fun resolveCodeLens(unresolved: CodeLens): CompletableFuture<CodeLens> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun didOpen(params: DidOpenTextDocumentParams) {
        workspace.onTextDocumentOpened(params)
        context.computeDiagnostics(params.textDocument.uri)
    }

    override fun didChange(params: DidChangeTextDocumentParams) {
        workspace.onTextDocumentChanged(params)
        context.computeDiagnostics(params.textDocument.uri)
    }

    override fun didSave(params: DidSaveTextDocumentParams) {
        workspace.onTextDocumentSaved(params)
        context.computeDiagnostics(params.textDocument.uri)
    }

    override fun didClose(params: DidCloseTextDocumentParams) {
        workspace.onTextDocumentClosed(params)
    }
}
