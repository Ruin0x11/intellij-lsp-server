package com.ruin.lsp.commands.document.find

import com.ruin.lsp.JAVA_PROJECT
import org.eclipse.lsp4j.Position

abstract class FindDefinitionCommandTestBase : FindCommandTestBase() {
    override val projectName = JAVA_PROJECT

    override fun command(at: Position, uri: String) =  FindDefinitionCommand(at)
}
