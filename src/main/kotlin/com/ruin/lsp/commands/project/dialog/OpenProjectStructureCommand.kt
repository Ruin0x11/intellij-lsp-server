package com.ruin.lsp.commands.project.dialog

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.options.ShowSettingsUtil
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ui.configuration.ProjectStructureConfigurable
import com.ruin.lsp.commands.ProjectCommand

class OpenProjectStructureCommand : ProjectCommand<Boolean> {
    override fun execute(ctx: Project): Boolean {
        ApplicationManager.getApplication().invokeLater {
            val configurable = ProjectStructureConfigurable.getInstance(ctx)
            ShowSettingsUtil.getInstance().editConfigurable(ctx, configurable)
        }
        return true
    }
}
