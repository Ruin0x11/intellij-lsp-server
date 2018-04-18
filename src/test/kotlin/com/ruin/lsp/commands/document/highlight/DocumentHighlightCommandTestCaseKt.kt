package com.ruin.lsp.commands.document.highlight

import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.KOTLIN_PROJECT
import com.ruin.lsp.forKotlin
import org.eclipse.lsp4j.DocumentHighlight
import org.eclipse.lsp4j.DocumentHighlightKind
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.Range

class DocumentHighlightCommandTestCaseKt : DocumentHighlightCommandTestBase() {
    override val projectName: String
        get() = KOTLIN_PROJECT

    fun `test highlights variable`() = checkHighlightsFound(forKotlin(DUMMY_FILE_PATH),
        Position(14, 15), listOf(
        // Declaration
        DocumentHighlight(
            Range(Position(14, 12), Position(14, 16)),
            DocumentHighlightKind.Write),
        // Method
        DocumentHighlight(
            Range(Position(15, 8), Position(15, 12)),
            DocumentHighlightKind.Read)
    ))

    fun `test highlights function`() = checkHighlightsFound(forKotlin(DUMMY_FILE_PATH),
        Position(42, 16), listOf(
        // Declaration
        DocumentHighlight(
            Range(Position(25, 17), Position(25, 26)),
            DocumentHighlightKind.Text),
        // Call
        DocumentHighlight(
            Range(Position(41, 8), Position(41, 17)),
            DocumentHighlightKind.Text),
        // Call
        DocumentHighlight(
            Range(Position(42, 8), Position(42, 17)),
            DocumentHighlightKind.Text)
    ))

}
