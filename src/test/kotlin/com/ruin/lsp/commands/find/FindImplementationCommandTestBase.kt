package com.ruin.lsp.commands.find

import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.commands.Command
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.TextDocumentIdentifier

abstract class FindImplementationCommandTestBase : FindCommandTestBase() {
    override val projectName: String
        get() = JAVA_PROJECT

    override fun command(at: Position, uri: String) =
        FindImplementationCommand(TextDocumentIdentifier(uri), at)
}
