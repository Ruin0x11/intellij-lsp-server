package com.ruin.intel.commands.highlight

import com.ruin.intel.DUMMY_FILE_PATH
import com.ruin.intel.values.DocumentHighlight
import com.ruin.intel.values.DocumentHighlightKind
import com.ruin.intel.values.Position
import com.ruin.intel.values.Range

class DocumentHighlightCommandTestCase : DocumentHighlightCommandTestBase() {
    fun `test highlights variable`() = checkHighlightsFound(DUMMY_FILE_PATH,
        Position(14, 27), listOf(
        // Declaration
        DocumentHighlight(
            Range(Position(14, 26), Position(14, 30)),
            DocumentHighlightKind.WRITE),
        // Method
        DocumentHighlight(
            Range(Position(15, 8), Position(15, 12)),
            DocumentHighlightKind.READ)
    ))

    fun `test highlights function`() = checkHighlightsFound(DUMMY_FILE_PATH,
        Position(46, 11), listOf(
        // Declaration
        DocumentHighlight(
            Range(Position(25, 9), Position(25, 18)),
            DocumentHighlightKind.TEXT),
        // Call
        DocumentHighlight(
            Range(Position(45, 8), Position(45, 17)),
            DocumentHighlightKind.TEXT),
        // Call
        DocumentHighlight(
            Range(Position(46, 8), Position(46, 17)),
            DocumentHighlightKind.TEXT)
    ))

}
