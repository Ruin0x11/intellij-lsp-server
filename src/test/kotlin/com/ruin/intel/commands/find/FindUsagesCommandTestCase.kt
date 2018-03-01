package com.ruin.intel.commands.find

import com.ruin.intel.DUMMY_FILE_PATH
import com.ruin.intel.values.Position

class FindUsagesCommandTestCase : FindUsagesCommandTestBase() {
    fun `test finds no usages`() = checkFindsNothing(DUMMY_FILE_PATH,
        Position(39, 6))

    fun `test finds multiple usages`() = checkFindsLocation(DUMMY_FILE_PATH,
        Position(25, 11), "Dummy.java", Position(45, 8))
}
