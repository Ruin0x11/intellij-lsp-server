package com.ruin.lsp.commands.document.action

import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import org.eclipse.lsp4j.CodeActionContext
import org.eclipse.lsp4j.Command
import org.eclipse.lsp4j.Range

class CodeActionCommand(val range: Range, val context: CodeActionContext) : DocumentCommand<MutableList<Command>> {
    override fun execute(ctx: ExecutionContext): MutableList<Command> {
        return mutableListOf()
    }
}
