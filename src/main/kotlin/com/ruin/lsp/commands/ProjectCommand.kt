package com.ruin.lsp.commands

import com.intellij.openapi.project.Project
import com.ruin.lsp.commands.project.dialog.OpenProjectStructureCommand
import com.ruin.lsp.commands.project.dialog.OpenRunConfigurationsCommand
import com.ruin.lsp.commands.project.dialog.ToggleFrameVisibilityCommand

/**
 * A command that is run on an entire project.
 */
interface ProjectCommand<out T : Any?> : Command<T, Project> {
    companion object {
        fun from(name: String): ProjectCommand<Any>? =
            when (name) {
                "openProjectStructure" -> OpenProjectStructureCommand()
                "openRunConfigurations" -> OpenRunConfigurationsCommand()
                "toggleFrameVisibility" -> ToggleFrameVisibilityCommand()
                else -> null
            }
    }
}
