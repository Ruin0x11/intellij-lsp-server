package com.ruin.lsp.commands.document.completion

import org.eclipse.lsp4j.Position

class CompletionCommandTestCase : CompletionCommandTestBase() {
    fun `test function completion`() =
        checkContainsCompletion(Position(13, 29), false, "boring() : void")

    fun `test function completion with parameter`() =
        checkContainsCompletion(Position(46, 32), false, "answerQuestion(String question) : int")

    fun `test function snippet`() =
        checkContainsCompletion(Position(13, 29), true, "notBoring(${'$'}${'{'}1:number${'}'})${'$'}0")

    fun `test variable completion`() =
        checkContainsCompletion(Position(15, 12), false,"list : ArrayList<String>")

    fun `test class completion`() =
        checkContainsCompletion(Position(13, 17), false,"org.lsp.javaproject.Dummy")
}
