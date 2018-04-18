package com.ruin.lsp.commands.project.run

import com.intellij.execution.RunManager
import com.intellij.execution.RunnerAndConfigurationSettings
import com.intellij.openapi.project.Project
import com.ruin.lsp.commands.ProjectCommand
import com.ruin.lsp.model.RunConfigurationDescription

class RunConfigurationsCommand : ProjectCommand<MutableList<RunConfigurationDescription>> {
    override fun execute(ctx: Project): MutableList<RunConfigurationDescription> {
        val runManager = RunManager.getInstance(ctx)
        val configs = runManager.allSettings

        return configs.map(RunnerAndConfigurationSettings::description).toMutableList()
    }
}

fun RunnerAndConfigurationSettings.description() =
    RunConfigurationDescription(
        id = this.uniqueID,
        name = this.name,
        configType = this.configuration.type.displayName
    )
