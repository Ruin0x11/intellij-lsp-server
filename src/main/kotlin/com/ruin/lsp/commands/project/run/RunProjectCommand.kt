package com.ruin.lsp.commands.project.run

import com.intellij.execution.RunManager
import com.intellij.execution.application.ApplicationConfiguration
import com.intellij.execution.configurations.*
import com.intellij.execution.executors.DefaultRunExecutor
import com.intellij.execution.impl.DefaultJavaProgramRunner
import com.intellij.execution.impl.RunManagerImpl
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.execution.runners.ExecutionEnvironmentBuilder
import com.intellij.openapi.application.TransactionGuard
import com.intellij.openapi.compiler.CompilerManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Ref
import com.intellij.task.ModuleBuildTask
import com.intellij.task.ProjectTask
import com.intellij.task.ProjectTaskManager
import com.intellij.task.impl.ProjectTaskList
import com.ruin.lsp.commands.ProjectCommand
import com.ruin.lsp.model.RunProjectCommandLine
import org.jetbrains.kotlin.idea.run.KotlinRunConfiguration
import org.slf4j.LoggerFactory

private val LOG = LoggerFactory.getLogger(RunProjectCommand::class.java)

class RunProjectCommand(private val id: String) : ProjectCommand<RunProjectCommandLine> {
    override fun execute(ctx: Project): RunProjectCommandLine {
        val runManager = RunManager.getInstance(ctx) as RunManagerImpl
        val setting = runManager.getConfigurationById(id) ?: return RunProjectCommandLine(false)
        val config = setting.configuration

        // CompileStepBeforeRun.doMake
        // CompilerManagerImpl.isUpToDate
        val needsRebuild = needsRebuild(ctx, config)
        val executor = DefaultRunExecutor.getRunExecutorInstance()
        val runner = DefaultJavaProgramRunner.getInstance() as DefaultJavaProgramRunner

        when(config) {
            is ApplicationConfiguration, is KotlinRunConfiguration -> {
                val env = ExecutionEnvironmentBuilder.create(config.project, executor, config).build()
                val state = config.getState(executor, env) ?: return RunProjectCommandLine(false)
                when (state) {
                    is JavaCommandLine -> {
                        runner.patch(state.javaParameters, env.runnerSettings, env.runProfile, true)
                        val classpath = state.javaParameters.classPath.pathsString
                        state.javaParameters.classPath.clear()
                        val line = state.javaParameters.toCommandLine()
                        return RunProjectCommandLine(needsRebuild, line.preparedCommandLine, line.workDirectory.absolutePath, classpath)
                    }
                }
            }
        }

        return RunProjectCommandLine(false)
    }
}

fun needsRebuild(project: Project, config: RunConfiguration): Boolean {
    if (config !is RunProfileWithCompileBeforeLaunchOption) {
        return false
    }

    if (config is RunConfigurationBase && (config as RunConfigurationBase).excludeCompileBeforeLaunchOption()) {
        return false
    }

    val task = modulesBuildTask(project, config as RunProfileWithCompileBeforeLaunchOption, false) ?: return false

    val compilerManager =  CompilerManager.getInstance(project)

    return if (task is ProjectTaskList) {
        task.map { (it as ModuleBuildTask).module }.toTypedArray()
            .let { compilerManager.isUpToDate(
                compilerManager.createModulesCompileScope(it, true)) }
    } else {
        compilerManager.isUpToDate(compilerManager.createModuleCompileScope((task as ModuleBuildTask).module, true))
    }
}

fun modulesBuildTask(project: Project, runConfiguration: RunProfileWithCompileBeforeLaunchOption, forceMakeProject: Boolean): ProjectTask? {
    val result: Ref<ProjectTask> = Ref(null)

    TransactionGuard.submitTransaction(project, Runnable {
        val projectTask: ProjectTask
        val projectTaskManager = ProjectTaskManager.getInstance(project)

        if (forceMakeProject) {
            // user explicitly requested whole-project make
            projectTask = projectTaskManager.createAllModulesBuildTask(true, project)
        } else {
            val modules = runConfiguration.modules
            projectTask = if (modules.isNotEmpty()) {
                for (module in modules) {
                    if (module == null) {
                        LOG.error("RunConfiguration should not return null modules. Configuration=" + runConfiguration.name + "; class=" +
                            runConfiguration.javaClass.name)
                    }
                }
                projectTaskManager.createModulesBuildTask(modules, true, true, true)
            } else {
                projectTaskManager.createAllModulesBuildTask(true, project)
            }
        }

        result.set(projectTask)
    })

    return result.get()
}

class CommandLineCollectingCommandLineState<T: ApplicationConfiguration>(config: T, env: ExecutionEnvironment)
    : ApplicationConfiguration.JavaApplicationCommandLineState<T>(config, env) {
    fun commandLine() = createCommandLine()
}
