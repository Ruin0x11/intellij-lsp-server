package com.ruin.lsp.commands.document.find

import com.ruin.lsp.*
import org.eclipse.lsp4j.Position

class FindImplementationCommandTestCaseKt : FindImplementationCommandTestBase() {
    override val projectName: String
        get() = KOTLIN_PROJECT

    fun `test finds single impl`() = checkFindsLocation(forKotlin(DUMMY_FILE_PATH),
        Position(8, 14), "SubClass.kt", Position(8, 19))

    fun `test finds impl of interface`() = checkFindsLocation(forKotlin(INTERFACE_FILE_PATH),
        Position(4, 15), "SubClass.kt", Position(6, 6))

    fun `test finds no impl of subclass`() = checkFindsNothing(forKotlin(SUBCLASS_FILE_PATH),
        Position(6, 11))
}
