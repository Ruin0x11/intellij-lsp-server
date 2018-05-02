package com.ruin.lsp.commands.document.find

import com.ruin.lsp.CONSTANTS_FILE_PATH
import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.JAVA_PROJECT
import org.eclipse.lsp4j.Position

class FindTypeDefinitionCommandTestCase : FindTypeDefinitionCommandTestBase() {
    override val projectName: String
        get() = JAVA_PROJECT

    fun `test finds declaration of class outside file`() =
        checkFindsLocation(DUMMY_FILE_PATH,
            Position(22, 24), "Problematic.java", Position(5, 13))

    fun `test finds declaration of class from declaration`() =
        checkFindsLocation(CONSTANTS_FILE_PATH,
            Position(5, 33), "EnumType.java", Position(2, 12))
}
