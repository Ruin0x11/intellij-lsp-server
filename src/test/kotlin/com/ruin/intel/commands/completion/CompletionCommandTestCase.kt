package com.ruin.intel.commands.completion

class CompletionCommandTestCase : CompletionCommandTestBase() {
    fun `test function completion`() =
        checkContainsCompletion(13, 29, false,"boring() -> void")
    fun `test function snippet`() =
        checkContainsCompletion(13, 29, true,"notBoring(${'$'}${'{'}1:number${'}'})${'$'}0")
}
