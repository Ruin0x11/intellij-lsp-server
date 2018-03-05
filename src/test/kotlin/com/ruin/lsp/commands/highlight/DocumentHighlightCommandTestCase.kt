package com.ruin.lsp.commands.highlight

import com.ruin.lsp.DUMMY_FILE_PATH
import org.eclipse.lsp4j.DocumentHighlight
import org.eclipse.lsp4j.DocumentHighlightKind
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.Range

class DocumentHighlightCommandTestCase : DocumentHighlightCommandTestBase() {
    fun `test highlights variable`() = checkHighlightsFound(DUMMY_FILE_PATH,
        Position(14, 27), listOf(
        // Declaration
        DocumentHighlight(
            Range(Position(14, 26), Position(14, 30)),
            DocumentHighlightKind.Write),
        // Method
        DocumentHighlight(
            Range(Position(15, 8), Position(15, 12)),
            DocumentHighlightKind.Read)
    ))

    fun `test highlights function`() = checkHighlightsFound(DUMMY_FILE_PATH,
        Position(46, 11), listOf(
        // Declaration
        DocumentHighlight(
            Range(Position(25, 9), Position(25, 18)),
            DocumentHighlightKind.Text),
        // Call
        DocumentHighlight(
            Range(Position(45, 8), Position(45, 17)),
            DocumentHighlightKind.Text),
        // Call
        DocumentHighlight(
            Range(Position(46, 8), Position(46, 17)),
            DocumentHighlightKind.Text)
    ))

}
