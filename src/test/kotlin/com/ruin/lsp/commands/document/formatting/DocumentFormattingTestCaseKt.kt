package com.ruin.lsp.commands.document.formatting

import com.ruin.lsp.range
import org.eclipse.lsp4j.FormattingOptions
import org.jetbrains.kotlin.idea.KotlinFileType

class DocumentFormattingTestCaseKt : DocumentFormattingTestBase() {
    override val fileType = KotlinFileType.INSTANCE

    val TESTCASE = """
        fun      main(args  : Array<  String   >) {
        println(  " Hey, dood! "  )
        }
"""

    fun `test formats whole document`() = checkDocumentFormat(TESTCASE,
        """fun main(args: Array<String>) {
    println(" Hey, dood! ")
}
""", FormattingOptions(2, true))
    fun `test formats whole document with tabs`() = checkDocumentFormat(TESTCASE,
        """fun main(args: Array<String>) {
		println(" Hey, dood! ")
}
""", FormattingOptions(2, false))

    fun `test formats range`() = checkDocumentFormat(TESTCASE,
        """
        fun      main(args  : Array<  String   >) {
            println(" Hey, dood! ")
        }
""", FormattingOptions(2, true), range(2, 0, 3, 0))
}
