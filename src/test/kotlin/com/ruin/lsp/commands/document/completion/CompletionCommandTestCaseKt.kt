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
        checkContainsCompletion(Position(13, 30), false, "boring() : Unit")

    fun `test function completion with parameter`() =
        checkContainsCompletion(Position(42, 32), false, "answerQuestion(question: String) : Int")

    fun `test function snippet`() =
        checkContainsCompletion(Position(16, 17), true, "notBoring(${'$'}${'{'}1:number${'}'})${'$'}0")

    fun `test variable completion`() =
        checkContainsCompletion(Position(15, 12), false,"list : [ERROR : Type for List<String>()]")

    fun `test class completion`() =
        checkContainsCompletion(Position(13, 13), false,"org.lsp.kotlinproject.Dummy")
}
