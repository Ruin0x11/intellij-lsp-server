package com.ruin.intel.commands.find

import com.ruin.intel.DUMMY_FILE_PATH
import com.ruin.intel.INTERFACE_FILE_PATH
import com.ruin.intel.SUBCLASS_FILE_PATH
import com.ruin.intel.values.Position

class FindImplementationTestCase : FindImplementationTestBase() {
    fun `test finds single impl`() = checkFindsLocation(DUMMY_FILE_PATH,
        Position(8, 14), "SubClass.java", Position(7, 17))

    fun `test finds impl of interface`() = checkFindsLocation(INTERFACE_FILE_PATH,
        Position(5, 18), "SubClass.java", Position(5, 13))

    fun `test finds no impl of subclass`() = checkFindsNothing(SUBCLASS_FILE_PATH,
        Position(5, 13))
}
