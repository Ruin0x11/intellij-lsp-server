package com.ruin.lsp.model

import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.services.LanguageClient

interface MyLanguageClient : LanguageClient {
    @JsonNotification("idea/notifyIndexingStarted")
    fun notifyIndexingStarted()

    @JsonNotification("idea/notifyIndexingEnded")
    fun notifyIndexingEnded()
}
