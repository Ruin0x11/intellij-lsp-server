package com.ruin.lsp.model

import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.services.LanguageClient

interface MyLanguageClient : LanguageClient {
    @JsonNotification("idea/indexStarted")
    fun notifyIndexingStarted()

    @JsonNotification("idea/indexEnded")
    fun notifyIndexingEnded()
}
