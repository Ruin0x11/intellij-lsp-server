package com.ruin.lsp.commands.find

import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.commands.Command
import com.ruin.lsp.values.Location
import com.ruin.lsp.values.Position
import com.ruin.lsp.values.TextDocumentIdentifier

abstract class FindUsagesCommandTestBase : FindCommandTestBase() {
    override val projectName = JAVA_PROJECT

    override fun command(at: Position, uri: String): Command<List<Location>> =
        FindUsagesCommand(TextDocumentIdentifier(uri), at)
}
