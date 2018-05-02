package com.ruin.lsp.commands.project.dialog

import com.intellij.execution.impl.EditConfigurationsDialog
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.project.Project
import com.ruin.lsp.commands.ProjectCommand

class OpenRunConfigurationsCommand : ProjectCommand<Boolean> {
    override fun execute(ctx: Project): Boolean {
        ApplicationManager.getApplication().invokeLater {
            val dialog = EditConfigurationsDialog(ctx)
            dialog.show()
        }
        return true
    }
}
