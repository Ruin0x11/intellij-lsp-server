package com.ruin.intel.commands.completion

class CompletionCommandTestCase : CompletionCommandTestBase() {
    fun `test function completion`() = checkContainsCompletion(12, 29, "boring")
}
