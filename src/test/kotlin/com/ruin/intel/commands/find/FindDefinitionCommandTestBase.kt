package com.ruin.intel.commands.find

import com.ruin.intel.JAVA_PROJECT
import com.ruin.intel.values.Position
import com.ruin.intel.values.TextDocumentIdentifier

abstract class FindDefinitionCommandTestBase : FindCommandTestBase() {
    override val projectName: String
        get() = JAVA_PROJECT

    override fun command(at: Position, uri: String) =  FindDefinitionCommand(TextDocumentIdentifier(uri), at)
}
