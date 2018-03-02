package com.ruin.intel.commands.find

import com.ruin.intel.JAVA_PROJECT
import com.ruin.intel.commands.Command
import com.ruin.intel.values.Location
import com.ruin.intel.values.Position
import com.ruin.intel.values.TextDocumentIdentifier

abstract class FindImplementationCommandTestBase : FindCommandTestBase() {
    override val projectName: String
        get() = JAVA_PROJECT

    override fun command(at: Position, uri: String): Command<List<Location>> =
        FindImplementationCommand(TextDocumentIdentifier(uri), at)
}
