package com.ruin.intel.commands.definition

import com.ruin.intel.DUMMY_FILE_PATH
import com.ruin.intel.SUBCLASS_FILE_PATH
import com.ruin.intel.values.Position

class DefinitionCommandTestCase : DefinitionCommandTestBase() {
    fun `test finds superclass from subclass`() =
        checkFindsDefinition(SUBCLASS_FILE_PATH, 5, 32, "SuperClass.java", Position(5, 22))

    fun `test finds declaration from usage`() =
        checkFindsDefinition(DUMMY_FILE_PATH, 46, 23, "Dummy.java", Position(49, 15))

    fun `test finds declaration of class outside file`() =
        checkFindsDefinition(DUMMY_FILE_PATH, 22, 9, "Problematic.java", Position(5, 13))
}
