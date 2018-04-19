package com.ruin.lsp.commands.project.run

import com.intellij.execution.RunManager
import com.intellij.execution.configurations.RunConfigurationBase
import com.intellij.execution.configurations.RunProfileWithCompileBeforeLaunchOption
import com.intellij.execution.impl.ExecutionManagerImpl
import com.intellij.execution.impl.RunManagerImpl
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.openapi.application.TransactionGuard
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.impl.VirtualFileManagerImpl
import com.intellij.task.*
import com.intellij.util.concurrency.Semaphore
import com.ruin.lsp.commands.ProjectCommand
import com.ruin.lsp.model.BuildProjectResult
import org.slf4j.LoggerFactory

private val LOG = LoggerFactory.getLogger(BuildProjectCommand::class.java)

class BuildProjectCommand(private val id: String,
                          private val forceMakeProject: Boolean,
                          private val ignoreErrors: Boolean) : ProjectCommand<BuildProjectResult> {
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
            val callback = ProjectTaskNotification { executionResult ->
                if ((executionResult.errors == 0 || ignoreErrors) && !executionResult.isAborted) {
                    LOG.info("===== Compile Finished =====")
                } else {
                    LOG.info("!!!!!  Compile Failed  !!!!!\n$executionResult")
                }
                //done.up()
            }

            TransactionGuard.submitTransaction(ctx, Runnable {
                val sessionId = ExecutionManagerImpl.EXECUTION_SESSION_ID_KEY.get(env)
                val projectTaskManager = ProjectTaskManager.getInstance(ctx)
                if (!ctx.isDisposed) {
                    projectTaskManager.run(ProjectTaskContext(sessionId, config), projectTask, callback)
                } else {
                   //done.up()
                }
            })
            // can't wait here, as CompileDriver will try to run the callback that releases the semaphore on the EDT
            // after the compile finishes, which causes a deadlock as we're already in the EDT.
            //done.waitFor()
        } catch (e: Exception) {
            return BuildProjectResult(false)
        }

        return BuildProjectResult(true)
    }
}
