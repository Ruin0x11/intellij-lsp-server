package com.ruin.lsp.commands.document.find

import com.ruin.lsp.CONSTANTS_FILE_PATH
import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.KOTLIN_PROJECT
import com.ruin.lsp.forKotlin
import org.eclipse.lsp4j.Position

class FindTypeDefinitionCommandTestCaseKt : FindTypeDefinitionCommandTestBase() {
    override val projectName: String
        get() = KOTLIN_PROJECT

    fun `test finds declaration of class outside file`() =
        checkFindsLocation(forKotlin(DUMMY_FILE_PATH),
            Position(22, 14), "Problematic.kt", Position(5, 6))

    fun `test finds declaration of class from declaration`() =
        checkFindsLocation(forKotlin(CONSTANTS_FILE_PATH),
            Position(11, 18), "EnumType.kt", Position(2, 11))
}
