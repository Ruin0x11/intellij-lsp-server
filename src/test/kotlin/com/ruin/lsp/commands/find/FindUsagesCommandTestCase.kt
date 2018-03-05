package com.ruin.lsp.commands.find

import com.ruin.lsp.DUMMY_FILE_PATH
import org.eclipse.lsp4j.Position

class FindUsagesCommandTestCase : FindUsagesCommandTestBase() {
    fun `test finds no usages`() = checkFindsNothing(DUMMY_FILE_PATH,
        Position(39, 6))

    fun `test finds multiple usages`() = checkFindsLocation(DUMMY_FILE_PATH,
        Position(25, 11), "Dummy.java", Position(45, 8))
}
