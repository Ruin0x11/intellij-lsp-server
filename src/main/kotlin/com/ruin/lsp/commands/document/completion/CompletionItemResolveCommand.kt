package com.ruin.lsp.commands.document.completion

import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.model.CompletionResolveIndex
import com.ruin.lsp.model.PreviousCompletionCacheService
import org.eclipse.lsp4j.CompletionItem

class CompletionItemResolveCommand(val item: CompletionItem)  : DocumentCommand<CompletionItem> {
    override fun execute(ctx: ExecutionContext): CompletionItem {
        val completionCache = PreviousCompletionCacheService.getInstance()
        val lookupElement = completionCache.resolveItem(item.data as CompletionResolveIndex) ?: return item
        return item
    }
}
