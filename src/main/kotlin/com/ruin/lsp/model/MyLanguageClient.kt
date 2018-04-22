package com.ruin.lsp.model

import com.ruin.lsp.values.DocumentUri
import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.services.LanguageClient

/**
 * Extensions to the client-side LSP protocol for IDEA-specific features.
 */
interface MyLanguageClient : LanguageClient {
    /**
     * The idea/indexStarted notification is sent from the server to the client to notify
     * the client that IDEA has started indexing files.
     */
    @JsonNotification("idea/indexStarted")
    fun notifyIndexStarted()

    /**
     * The idea/indexFinished notification is sent from the server to the client to notify
     * the client that IDEA has finished indexing files.
     */
    @JsonNotification("idea/indexFinished")
    fun notifyIndexFinished()

    /**
     * The idea/buildMessages notification is sent from the server to the client to notify
     * the client that a single project build task has completed with error/warning/information messages.
     */
    @JsonNotification("idea/buildMessages")
    fun notifyBuildMessages(messages: List<BuildMessages>)

    /**
     * The idea/buildFinished notification is sent from the server to the client to notify
     * the client that all tasks in a project build have finished.
     */
    @JsonNotification("idea/buildFinished")
    fun notifyBuildFinished(result: BuildResult)
}

data class BuildResult(val errors: Int, val warnings: Int, val isAborted: Boolean)
data class BuildMessages(val uri: DocumentUri, val diagnostics: List<Diagnostic>)
