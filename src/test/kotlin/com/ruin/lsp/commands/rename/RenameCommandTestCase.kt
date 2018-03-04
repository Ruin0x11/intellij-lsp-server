package com.ruin.lsp.commands.rename

import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.SUBCLASS_FILE_PATH
import com.ruin.lsp.values.Position
import com.ruin.lsp.values.Range
import com.ruin.lsp.values.TextEdit

class RenameCommandTestCase : RenameCommandTestBase() {
    fun `test rename class`() = checkRenameHas(DUMMY_FILE_PATH, Position(8, 15), "MyClass",
        listOf(
            Pair(DUMMY_FILE_PATH, listOf(
                TextEdit(Range(Position(8, 13), Position(8, 18)), "MyClass"),
                TextEdit(Range(Position(13, 12), Position(13, 17)), "MyClass"),
                TextEdit(Range(Position(27, 11), Position(27, 16)), "MyClass"),
                TextEdit(Range(Position(33, 4), Position(33, 9)), "MyClass"),
                TextEdit(Range(Position(35, 4), Position(35, 9)), "MyClass"),
                TextEdit(Range(Position(37, 4), Position(37, 9)), "MyClass"),
                TextEdit(Range(Position(39, 4), Position(39, 9)), "MyClass")
            )),
            Pair(SUBCLASS_FILE_PATH, listOf(
                TextEdit(Range(Position(7, 37), Position(7, 43)), "MyClass")
            ))
        ))

    fun `test rename method`() = checkRenameHas(DUMMY_FILE_PATH, Position(21, 16), "myMethod",
        listOf(
            Pair(DUMMY_FILE_PATH, listOf(
                TextEdit(Range(Position(16, 8), Position(16, 17)), "myMethod"),
                TextEdit(Range(Position(21, 16), Position(21, 24)), "myMethod"),
                TextEdit(Range(Position(25, 9), Position(25, 17)), "myMethod"),
                TextEdit(Range(Position(45, 8), Position(45, 16)), "myMethod"),
                TextEdit(Range(Position(46, 8), Position(46, 16)), "myMethod")
            ))
        ))

    fun `test rename variable`() = checkRenameHas(DUMMY_FILE_PATH, Position(14, 26), "myVariable",
        listOf(
            Pair(DUMMY_FILE_PATH, listOf(
                TextEdit(Range(Position(14, 26), Position(14, 30)), "myVariable"),
                TextEdit(Range(Position(15, 8), Position(15, 12)), "myVariable")
            ))
        ))

    fun `test rename field`() = checkRenameHas(DUMMY_FILE_PATH, Position(9, 17), "myField",
        listOf(
            Pair(DUMMY_FILE_PATH, listOf(
                TextEdit(Range(Position(9, 16), Position(9, 22)), "myField"),
                TextEdit(Range(Position(17, 8), Position(17, 14)), "myField")
                ))
        ))
}
