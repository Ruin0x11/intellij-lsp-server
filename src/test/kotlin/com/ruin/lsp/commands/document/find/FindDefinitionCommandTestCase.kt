package com.ruin.lsp.commands.document.find

import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.SUBCLASS_FILE_PATH
import org.eclipse.lsp4j.Position

class FindDefinitionCommandTestCase : FindDefinitionCommandTestBase() {
    override val projectName: String
        get() = JAVA_PROJECT

    fun `test finds superclass from subclass`() =
        checkFindsLocation(SUBCLASS_FILE_PATH,
            Position(5, 32), "SuperClass.java", Position(5, 22))

    fun `test finds declaration from usage`() =
        checkFindsLocation(DUMMY_FILE_PATH,
            Position(46, 23), "Dummy.java", Position(49, 15))

    fun `test finds declaration of class outside file`() =
        checkFindsLocation(DUMMY_FILE_PATH,
            Position(22, 9), "Problematic.java", Position(5, 13))

    fun `test finds super method`() =
        checkFindsLocation(SUBCLASS_FILE_PATH,
            Position(12, 25), "SuperClass.java", Position(9, 25))
}
