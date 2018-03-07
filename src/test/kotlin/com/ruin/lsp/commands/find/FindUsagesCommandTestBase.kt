package com.ruin.lsp.commands.find

import com.ruin.lsp.JAVA_PROJECT
import org.eclipse.lsp4j.Position

abstract class FindUsagesCommandTestBase : FindCommandTestBase() {
    override val projectName: String
        get() = JAVA_PROJECT

    override fun command(at: Position, uri: String) =
        FindUsagesCommand(at)
}
