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
 * A run configuration ID taken from IDEA's list of run configurations.
 */
typealias ConfigId = String

/**
 * A run configuration from IDEA. Contains user-displayable information and the ID used to run it.
 */
data class RunConfigurationDescription(val id: ConfigId, val name: String, val configType: String)

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

/**
 * A combination of a run configuration's description and last run state. Used with code lenses for generating
 * contextual information about a run configuration in a file.
 */
data class RunConfigurationData(val configuration: RunConfigurationDescription, val state: RunConfigurationState)

/**
 * A set of parameters for building a project with IDEA. Uses a configuration ID taken from an idea/runConfigurations
 * request.
 */
data class BuildProjectParams(val textDocument: TextDocumentIdentifier,
                              val id: ConfigId,
                              val forceMakeProject: Boolean,
                              val ignoreErrors: Boolean)

// TODO: Add build id.
/**
 * The result of a build operation. Allows the client to see if the build started successfully.
 */
data class BuildProjectResult(val started: Boolean)

/**
 * A set of parameters for getting a project's command line for running. Uses a configuration ID taken from an
 * idea/runConfigurations request.
 */
data class RunProjectParams(val textDocument: TextDocumentIdentifier, val id: ConfigId)

/**
 * A command line suitable for running the project from the client. If the flag isUpToDate is false, an
 * idea/buildProject request should be sent first with the configuration ID that was passed to the idea/runProject
 * command used to obtain the command line, then the client should run the command line only if the build succeeds.
 */
data class RunProjectCommandLine(val isUpToDate: Boolean,
                                 val command: String? = null,
                                 val workingDirectory: String? = null,
                                 val classpath: String? = null)
