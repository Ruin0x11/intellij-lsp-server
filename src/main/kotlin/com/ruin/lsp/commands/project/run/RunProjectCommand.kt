package com.ruin.lsp.commands.project.run

import com.intellij.execution.RunManager
import com.intellij.execution.application.ApplicationConfiguration
import com.intellij.execution.configurations.JavaCommandLine
import com.intellij.execution.executors.DefaultRunExecutor
import com.intellij.execution.impl.DefaultJavaProgramRunner
import com.intellij.execution.impl.RunManagerImpl
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.openapi.project.Project
import com.ruin.lsp.commands.ProjectCommand
import com.ruin.lsp.model.RunProjectCommandLine

class RunProjectCommand(private val id: String) : ProjectCommand<RunProjectCommandLine> {
    override fun execute(ctx: Project): RunProjectCommandLine {
        val runManager = RunManager.getInstance(ctx) as RunManagerImpl
        val setting = runManager.getConfigurationById(id) ?: return RunProjectCommandLine()
        val config = setting.configuration
        when(config) {
            is ApplicationConfiguration -> {
                val runner = DefaultJavaProgramRunner.getInstance() as DefaultJavaProgramRunner
                val executor = DefaultRunExecutor.getRunExecutorInstance()
                val env = ExecutionEnvironment(executor, runner, setting, ctx)
                val state = config.getState(executor, env) ?: return RunProjectCommandLine()
                when (state) {
                    is JavaCommandLine -> {
                        runner.patch(state.javaParameters, env.runnerSettings, env.runProfile, true)
                        val classpath = state.javaParameters.classPath.pathsString
                        state.javaParameters.classPath.clear()
                        val line = state.javaParameters.toCommandLine()
                        return RunProjectCommandLine(line.preparedCommandLine, line.workDirectory.absolutePath, classpath)
                    }
                }
            }
        }

        return RunProjectCommandLine()
    }
}

class CommandLineCollectingCommandLineState<T: ApplicationConfiguration>(config: T, env: ExecutionEnvironment)
    : ApplicationConfiguration.JavaApplicationCommandLineState<T>(config, env) {
    fun commandLine() = createCommandLine()
}
