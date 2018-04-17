package com.ruin.lsp.commands.document.find

import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.INTERFACE_FILE_PATH
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.SUBCLASS_FILE_PATH
import org.eclipse.lsp4j.Position

class FindImplementationCommandTestCase : FindImplementationCommandTestBase() {
    override val projectName: String
        get() = JAVA_PROJECT

    fun `test finds single impl`() = checkFindsLocation(DUMMY_FILE_PATH,
        Position(8, 14), "SubClass.java", Position(7, 17))

    fun `test finds impl of interface`() = checkFindsLocation(INTERFACE_FILE_PATH,
        Position(5, 18), "SubClass.java", Position(5, 13))

    fun `test finds no impl of subclass`() = checkFindsNothing(SUBCLASS_FILE_PATH,
        Position(5, 13))
}
