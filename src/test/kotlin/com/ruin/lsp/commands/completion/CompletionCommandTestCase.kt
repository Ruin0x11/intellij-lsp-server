package com.ruin.lsp.commands.completion

class CompletionCommandTestCase : CompletionCommandTestBase() {
    fun `test function completion`() =
        checkContainsCompletion(13, 29, false,"boring() -> void")

    fun `test function snippet`() =
        checkContainsCompletion(13, 29, true,"notBoring(${'$'}${'{'}1:number${'}'})${'$'}0")

    fun `test variable completion`() =
        checkContainsCompletion(15, 12, false,"list : ArrayList<String>")
}
