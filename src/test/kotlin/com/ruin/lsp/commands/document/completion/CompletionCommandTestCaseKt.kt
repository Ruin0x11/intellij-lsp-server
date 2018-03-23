package com.ruin.lsp.commands.document.completion

import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.KOTLIN_PROJECT
import com.ruin.lsp.forKotlin
import org.eclipse.lsp4j.Position

class CompletionCommandTestCaseKt : CompletionCommandTestBase() {
    override val projectName: String
        get() = KOTLIN_PROJECT

    override val filePath: String
        get() = forKotlin(DUMMY_FILE_PATH)

    fun `test function completion`() =
        checkContainsCompletion(Position(52, 15), false, "boring() : void")

    fun `test function completion with parameter`() =
        checkContainsCompletion(Position(42, 32), false, "answerQuestion(String question) : int")

    fun `test function snippet`() =
        checkContainsCompletion(Position(13, 25), true, "notBoring(${'$'}${'{'}1:number${'}'})${'$'}0")

    fun `test variable completion`() =
        checkContainsCompletion(Position(15, 12), false,"list : ArrayList<String>")

    fun `test class completion`() =
        checkContainsCompletion(Position(13, 13), false,"org.lsp.javaproject.Dummy")
}
