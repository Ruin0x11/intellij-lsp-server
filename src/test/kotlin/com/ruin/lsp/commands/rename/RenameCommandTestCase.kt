package com.ruin.lsp.commands.rename

import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.INTERFACE_FILE_PATH
import com.ruin.lsp.SUBCLASS_FILE_PATH
import com.ruin.lsp.range
import com.ruin.lsp.values.Position
import com.ruin.lsp.values.Range
import com.ruin.lsp.values.TextEdit

class RenameCommandTestCase : RenameCommandTestBase() {
    fun `test rename class`() = checkRenameHas(DUMMY_FILE_PATH, Position(8, 15), "MyClass",
        listOf(
            Pair(DUMMY_FILE_PATH, listOf(
                TextEdit(range(8, 13, 8, 18), "MyClass"),
                TextEdit(range(13, 12, 13, 17), "MyClass"),
                TextEdit(range(27, 11, 27, 16), "MyClass"),
                TextEdit(range(33, 4, 33, 9), "MyClass"),
                TextEdit(range(35, 4, 35, 9), "MyClass"),
                TextEdit(range(37, 4, 37, 9), "MyClass"),
                TextEdit(range(39, 4, 39, 9), "MyClass")
            )),
            Pair(SUBCLASS_FILE_PATH, listOf(
                TextEdit(range(7, 37, 7, 42), "MyClass")
            ))
        ))

    fun `test rename method`() = checkRenameHas(DUMMY_FILE_PATH, Position(21, 18), "myMethod",
        listOf(
            Pair(DUMMY_FILE_PATH, listOf(
                TextEdit(range(16, 8, 16, 17), "myMethod"),
                TextEdit(range(21, 16, 21, 25), "myMethod"),
                TextEdit(range(25, 9, 25, 18), "myMethod"),
                TextEdit(range(45, 8, 45, 17), "myMethod"),
                TextEdit(range(46, 8, 46, 17), "myMethod")
            ))
        ))

    fun `test rename method parameter`() = checkRenameHas(DUMMY_FILE_PATH, Position(53, 29), "myParameter",
        listOf(
            Pair(DUMMY_FILE_PATH, listOf(
                TextEdit(range(53, 26, 53, 32), "myParameter"),
                TextEdit(range(54, 22, 54, 28), "myParameter"),
                TextEdit(range(56, 8, 56, 14), "myParameter")
            ))
        ))

    fun `test rename variable`() = checkRenameHas(DUMMY_FILE_PATH, Position(14, 26), "myVariable",
        listOf(
            Pair(DUMMY_FILE_PATH, listOf(
                TextEdit(range(14, 26, 14, 30), "myVariable"),
                TextEdit(range(15, 8, 15, 12), "myVariable")
            ))
        ))

    fun `test rename field`() = checkRenameHas(DUMMY_FILE_PATH, Position(9, 17), "myField",
        listOf(
            Pair(DUMMY_FILE_PATH, listOf(
                TextEdit(range(9, 16, 9, 22), "myField"),
                TextEdit(range(17, 8, 17, 14), "myField"),
                TextEdit(range(54, 13, 54, 19), "myField"),
                TextEdit(range(55, 13, 55, 19), "myField"),
                TextEdit(range(56, 22, 56, 28), "myField"),
                TextEdit(range(60, 8, 60, 14), "myField")
            ))
        ))

    fun `test rename interface`() = checkRenameHas(INTERFACE_FILE_PATH, Position(5, 18), "CoolInterface",
        listOf(
            Pair(INTERFACE_FILE_PATH, listOf(
                TextEdit(range(5, 17, 5, 28), "CoolInterface")
            )),
            Pair(SUBCLASS_FILE_PATH, listOf(
                TextEdit(range(5, 52, 5, 63), "CoolInterface")
            ))
        ))
}
