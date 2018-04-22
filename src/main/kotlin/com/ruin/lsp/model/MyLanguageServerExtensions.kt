package com.ruin.lsp.model

import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.TextDocumentIdentifier
import org.eclipse.lsp4j.TextDocumentPositionParams
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest
import org.eclipse.lsp4j.jsonrpc.services.JsonSegment
import java.util.concurrent.CompletableFuture

/**
 * Extensions to the server-side LSP protocol for IDEA-specific features.
 */
@JsonSegment("idea")
interface MyLanguageServerExtensions {
    /**
     * The idea/implementations request is sent from the client to the server to
     * resolve the implementation locations of a symbol at a given text document
     * position.
     */
    @JsonRequest
    fun implementations(params: TextDocumentPositionParams): CompletableFuture<MutableList<Location>>

    /**
     * The idea/runConfigurations request is sent from the client to the server to
     * obtain the list of IDEA run configurations for the current project.
     */
    @JsonRequest
    fun runConfigurations(params: TextDocumentPositionParams): CompletableFuture<MutableList<RunConfigurationDescription>>

    /**
     * The idea/buildProject request is sent from the client to the server to
     * build the current project using the specified IDEA run configuration.
     */
    @JsonRequest
    fun buildProject(params: BuildProjectParams): CompletableFuture<BuildProjectResult>

    /**
     * The idea/runProject request is sent from the client to the server to
     * obtain a command line statement used for running the current project.
     *
     * The response also indicates whether or not the project's compiled classes
     * are out of date. If they are, the client should send the idea/buildProject
     * request first, record the build ID that it returns, and wait for an
     * idea/buildFinished notification with that ID before executing the run command line.
     */
    @JsonRequest
    fun runProject(params: RunProjectParams): CompletableFuture<RunProjectCommandLine>
}

/**
 * A run configuration from IntelliJ. Contains user-displayable information and the ID used to run it.
 */
data class RunConfigurationDescription(val id: String, val name: String, val configType: String)

/**
 * The state of a run configuration. For example, it can display whether or not a test has passed. It also
 * allows determining which configuration runs the whole class rather than an individual item.
 */
enum class RunConfigurationState(val ord: Int) {
    Run(1),
    RunClass(2),
    Test(3),
    TestPass(4),
    TestFail(5),
    TestUnknown(6)
}
data class RunConfigurationData(val configuration: RunConfigurationDescription, val state: RunConfigurationState)

data class BuildProjectParams(val textDocument: TextDocumentIdentifier,
                              val id: String,
                              val forceMakeProject: Boolean,
                              val ignoreErrors: Boolean)
data class BuildProjectResult(val started: Boolean)

data class RunProjectParams(val textDocument: TextDocumentIdentifier, val id: String)
data class RunProjectCommandLine(val isUpToDate: Boolean,
                                 val command: String? = null,
                                 val workingDirectory: String? = null,
                                 val classpath: String? = null)
