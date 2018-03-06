package com.ruin.lsp

import com.ruin.lsp.model.rangeToTextRange
import com.ruin.lsp.util.getDocument
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.Range
import org.intellivim.FileEditingTestCase

class RangeTest : FileEditingTestCase() {
    override val filePath: String
        get() = DUMMY_FILE_PATH

    override val projectName: String
        get() = JAVA_PROJECT

    private fun checkRangeEquals(startLine: Int, startChar: Int, endLine: Int, endChar: Int, expected: String) {
        val doc = getDocument(psiFile)!!

        val range = Range(
            Position(startLine, startChar),
            Position(endLine, endChar)
        )

        val textRange = rangeToTextRange(doc, range)

        assertEquals(expected, doc.getText(textRange))
    }

    fun `test range empty`() = checkRangeEquals(46, 10, 46, 10, "")
    fun `test range same line`() = checkRangeEquals(8, 0, 8, 6,
        "public")
    fun `test range line break`() = checkRangeEquals(17, 4, 18, 5,
        "    thingy = 12;\n    }")
    fun `test range multiple lines`() = checkRangeEquals(32,0,35,0,
        "\n    Dummy() {\n    }\n")
}
