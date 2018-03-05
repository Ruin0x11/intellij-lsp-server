package com.ruin.lsp.commands.find

import com.ruin.lsp.JAVA_PROJECT
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.TextDocumentIdentifier

abstract class FindDefinitionCommandTestBase : FindCommandTestBase() {
    override val projectName: String
        get() = JAVA_PROJECT

    override fun command(at: Position, uri: String) =  FindDefinitionCommand(TextDocumentIdentifier(uri), at)
}
