package com.ruin.intel.commands.highlight

import com.ruin.intel.DUMMY_FILE_PATH
import com.ruin.intel.values.DocumentHighlight
import com.ruin.intel.values.DocumentHighlightKind
import com.ruin.intel.values.Position
import com.ruin.intel.values.Range

class DocumentHighlightCommandTestCase : DocumentHighlightCommandTestBase() {
    fun `test highlights variable`() = checkHighlightsFound(DUMMY_FILE_PATH,
        Position(14, 27), listOf(
        DocumentHighlight(
            Range(Position(15, 7), Position(15, 11)),
            DocumentHighlightKind.READ)
    ))
}
