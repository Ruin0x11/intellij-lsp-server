package com.ruin.lsp.commands.project.dialog

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.project.Project
import com.ruin.lsp.commands.ProjectCommand
import com.ruin.lsp.util.toggleProjectFrame

class ToggleFrameVisibilityCommand : ProjectCommand<Boolean> {
    override fun execute(ctx: Project): Boolean {
        ApplicationManager.getApplication().runWriteAction { toggleProjectFrame(ctx) }
        return true
    }
}
