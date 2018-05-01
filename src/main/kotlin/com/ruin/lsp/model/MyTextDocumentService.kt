package com.ruin.lsp.model

import com.intellij.openapi.components.ServiceManager
import com.ruin.lsp.commands.document.completion.CompletionCommand
import com.ruin.lsp.commands.document.completion.CompletionItemResolveCommand
import com.ruin.lsp.commands.document.find.FindDefinitionCommand
import com.ruin.lsp.commands.document.find.FindImplementationCommand
import com.ruin.lsp.commands.document.find.FindTypeDefinitionCommand
import com.ruin.lsp.commands.document.find.FindUsagesCommand
import com.ruin.lsp.commands.document.formatting.DocumentFormattingCommand
import com.ruin.lsp.commands.document.highlight.DocumentHighlightCommand
import com.ruin.lsp.commands.document.hover.HoverCommand
import com.ruin.lsp.commands.document.lens.CodeLensCommand
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
        return server.asInvokeAndWaitFuture(server.context.rootProject!!, uri, command)
    }

    override fun codeAction(params: CodeActionParams): CompletableFuture<MutableList<out Command>> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun hover(position: TextDocumentPositionParams): CompletableFuture<Hover> =
        server.asInvokeAndWaitFuture(server.context.rootProject!!, position.textDocument.uri, HoverCommand(position.position))

    override fun documentHighlight(position: TextDocumentPositionParams): CompletableFuture<MutableList<out DocumentHighlight>> =
        server.asInvokeAndWaitFuture(server.context.rootProject!!, position.textDocument.uri, DocumentHighlightCommand(position.position))

    override fun definition(position: TextDocumentPositionParams): CompletableFuture<MutableList<out Location>> =
        server.asInvokeAndWaitFuture(server.context.rootProject!!, position.textDocument.uri, FindDefinitionCommand(position.position))

    override fun typeDefinition(position: TextDocumentPositionParams): CompletableFuture<MutableList<out Location>> =
        server.asInvokeAndWaitFuture(server.context.rootProject!!, position.textDocument.uri, FindTypeDefinitionCommand(position.position))

    override fun formatting(params: DocumentFormattingParams): CompletableFuture<MutableList<out TextEdit>> =
        server.asInvokeAndWaitFuture(server.context.rootProject!!, params.textDocument.uri, DocumentFormattingCommand(params.options))

    override fun rangeFormatting(params: DocumentRangeFormattingParams): CompletableFuture<MutableList<out TextEdit>> =
        server.asInvokeAndWaitFuture(server.context.rootProject!!, params.textDocument.uri, DocumentFormattingCommand(params.options, params.range))

    override fun onTypeFormatting(params: DocumentOnTypeFormattingParams): CompletableFuture<MutableList<out TextEdit>> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun codeLens(params: CodeLensParams): CompletableFuture<MutableList<out CodeLens>> =
        server.asInvokeAndWaitFuture(server.context.rootProject!!, params.textDocument.uri, CodeLensCommand())

    override fun rename(params: RenameParams): CompletableFuture<WorkspaceEdit> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun completion(params: CompletionParams): CompletableFuture<Either<MutableList<CompletionItem>, CompletionList>> =
        server.asCancellableInvokeAndWaitFuture(server.context.rootProject!!, params.textDocument.uri, CompletionCommand(params.position,
            server.context.clientCapabilities?.textDocument?.completion?.completionItem?.snippetSupport ?: false))

    override fun documentSymbol(params: DocumentSymbolParams): CompletableFuture<MutableList<out SymbolInformation>> =
        server.asCancellableInvokeAndWaitFuture(server.context.rootProject!!, params.textDocument.uri, DocumentSymbolCommand(params.textDocument))

    override fun signatureHelp(position: TextDocumentPositionParams): CompletableFuture<SignatureHelp> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun references(params: ReferenceParams): CompletableFuture<MutableList<out Location>> =
        server.asCancellableInvokeAndWaitFuture(server.context.rootProject!!, params.textDocument.uri, FindUsagesCommand(params.position))

    override fun implementation(params: TextDocumentPositionParams): CompletableFuture<List<Location>> =
        server.asInvokeAndWaitFuture(server.context.rootProject!!, params.textDocument.uri, FindImplementationCommand(params.position))

    override fun resolveCodeLens(unresolved: CodeLens): CompletableFuture<CodeLens> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun documentLink(params: DocumentLinkParams?): CompletableFuture<MutableList<DocumentLink>> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun documentLinkResolve(params: DocumentLink?): CompletableFuture<DocumentLink> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun willSaveWaitUntil(params: WillSaveTextDocumentParams?): CompletableFuture<MutableList<TextEdit>> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun willSave(params: WillSaveTextDocumentParams?) {}


    override fun didOpen(params: DidOpenTextDocumentParams) {
        workspace.onTextDocumentOpened(params, server.context.rootProject!!, server.context.client, server)
        server.computeDiagnostics(params.textDocument.uri)
    }

    override fun didChange(params: DidChangeTextDocumentParams) {
        workspace.onTextDocumentChanged(params, server.context.rootProject!!)
        server.computeDiagnostics(params.textDocument.uri)
    }

    override fun didSave(params: DidSaveTextDocumentParams) {
        workspace.onTextDocumentSaved(params, server.context.rootProject!!)
        server.computeDiagnostics(params.textDocument.uri)
    }

    override fun didClose(params: DidCloseTextDocumentParams) {
        workspace.onTextDocumentClosed(params)
    }
}
