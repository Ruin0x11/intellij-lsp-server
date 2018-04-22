package com.ruin.lsp.commands.project.run

import com.android.tools.idea.gradle.project.build.JpsBuildContext
import com.android.tools.idea.gradle.project.build.PostProjectBuildTasksExecutor
import com.android.tools.idea.project.AndroidProjectBuildNotifications
import com.intellij.compiler.CompilerMessageImpl
import com.intellij.execution.RunManager
import com.intellij.execution.configurations.RunConfigurationBase
import com.intellij.execution.configurations.RunProfileWithCompileBeforeLaunchOption
import com.intellij.execution.impl.ExecutionManagerImpl
import com.intellij.execution.impl.RunManagerImpl
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.openapi.application.TransactionGuard
import com.intellij.openapi.compiler.*
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.impl.VirtualFileManagerImpl
import com.intellij.task.*
import com.intellij.util.concurrency.Semaphore
import com.ruin.lsp.commands.ProjectCommand
import com.ruin.lsp.model.*
import com.ruin.lsp.util.getURIForFile
import org.apache.log4j.Level
import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.DiagnosticSeverity
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.Range
import org.slf4j.LoggerFactory
import java.lang.Compiler

private val LOG = Logger.getInstance(LanguageServerRunner::class.java)

class BuildProjectCommand(private val id: String,
                          private val forceMakeProject: Boolean,
                          private val ignoreErrors: Boolean,
                          private val client: MyLanguageClient) : ProjectCommand<BuildProjectResult> {
    override fun execute(ctx: Project): BuildProjectResult {
        val runManager = RunManager.getInstance(ctx) as RunManagerImpl
        val setting = runManager.getConfigurationById(id) ?: return BuildProjectResult(false)
        val config = setting.configuration

        if (config is RunConfigurationBase && config.excludeCompileBeforeLaunchOption()) {
            return BuildProjectResult(false)
        }

        val runConfiguration = config as RunProfileWithCompileBeforeLaunchOption

        val projectTask = modulesBuildTask(ctx, runConfiguration, forceMakeProject) ?: return BuildProjectResult(false)

        val env = ExecutionEnvironment()
        var id = env.executionId
        if (id == 0L) {
            id = env.assignNewExecutionId()
        }
        ExecutionManagerImpl.EXECUTION_SESSION_ID_KEY.set(env, id)

        VirtualFileManagerImpl.getInstance().syncRefresh()

        try {
            //val done = Semaphore()
            //done.down()
            TransactionGuard.submitTransaction(ctx, Runnable {
                val sessionId = ExecutionManagerImpl.EXECUTION_SESSION_ID_KEY.get(env)
                val projectTaskManager = MyProjectTaskManager(ctx, compileStatusNotification(client))
                if (!ctx.isDisposed) {
                    projectTaskManager.run(ProjectTaskContext(sessionId, config), projectTask, ProjectTaskNotification {
                        client.notifyBuildFinished(it.buildResult())
                    })
                } else {
                    //done.up()
                }
            })
            // can't wait here, as CompileDriver will try to run the callback that releases the semaphore on the EDT
            // after the compile finishes, which causes a deadlock as we're already on the EDT.
            //done.waitFor()
        } catch (e: Exception) {
            val writer = LogPrintWriter(LOG, Level.ERROR)
            e.printStackTrace(LogPrintWriter(LOG, Level.ERROR))
            writer.flush()
            return BuildProjectResult(false)
        }

        return BuildProjectResult(true)
    }

    private fun compileStatusNotification(client: MyLanguageClient): CompileStatusNotification {
        return CompileStatusNotification { _, _, _, compileContext ->
            client.notifyBuildMessages(compileContext.buildMessages())
        }
    }
}

private fun ProjectTaskResult.buildResult() =
    BuildResult(errors, warnings, isAborted)

private fun CompileContext.buildMessages(): List<BuildMessages> {
    val messages = mutableListOf<CompilerMessage>()
    CompilerMessageCategory.values().forEach { category ->
        val forCategory = this.getMessages(category)
        messages.addAll(forCategory)
    }

    return messages.groupBy { getURIForFile(it.virtualFile) }
        .map { (uri, messages) ->
            BuildMessages(uri, messages.map { mes -> (mes as CompilerMessageImpl).diagnostic() })
        }
}

private fun CompilerMessageCategory.diagnosticSeverity() =
    when (this) {
        CompilerMessageCategory.STATISTICS -> DiagnosticSeverity.Hint
        CompilerMessageCategory.INFORMATION -> DiagnosticSeverity.Information
        CompilerMessageCategory.WARNING -> DiagnosticSeverity.Warning
        CompilerMessageCategory.ERROR -> DiagnosticSeverity.Error
    }

private fun CompilerMessageImpl.position() = Position(line, column)

private fun CompilerMessageImpl.diagnostic() =
    Diagnostic(Range(position(), position()), message, category.diagnosticSeverity(), exportTextPrefix)

