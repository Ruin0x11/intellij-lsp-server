package com.ruin.lsp.model

import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.TextDocumentIdentifier
import org.eclipse.lsp4j.TextDocumentPositionParams
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest
import org.eclipse.lsp4j.jsonrpc.services.JsonSegment
import java.util.concurrent.CompletableFuture

@JsonSegment("idea")
interface MyLanguageServerExtensions {
    @JsonRequest fun implementations(params: TextDocumentPositionParams): CompletableFuture<MutableList<Location>>

    @JsonRequest fun runConfigurations(params: TextDocumentPositionParams): CompletableFuture<MutableList<RunConfigurationDescription>>

    @JsonRequest fun buildProject(params: BuildProjectParams): CompletableFuture<BuildProjectResult>

    @JsonRequest fun runProject(params: RunProjectParams): CompletableFuture<RunProjectCommandLine>

    @JsonRequest fun openProjectStructure(params: TextDocumentPositionParams): CompletableFuture<Boolean>

    @JsonRequest fun toggleFrameVisibility(params: TextDocumentPositionParams): CompletableFuture<Boolean>
}

data class RunConfigurationDescription(val id: String, val name: String, val configType: String)

data class BuildProjectParams(val textDocument: TextDocumentIdentifier,
                              val id: String,
                              val forceMakeProject: Boolean,
                              val ignoreErrors: Boolean)
data class BuildProjectResult(val started: Boolean)
data class RunProjectParams(val textDocument: TextDocumentIdentifier, val id: String)
data class RunProjectCommandLine(val needsRebuild: Boolean,
                                 val command: String? = null,
                                 val workingDirectory: String? = null,
                                 val classpath: String? = null)
