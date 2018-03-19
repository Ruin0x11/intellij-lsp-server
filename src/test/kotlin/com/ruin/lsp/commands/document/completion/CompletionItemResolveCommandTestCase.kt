package com.ruin.lsp.commands.document.completion

import org.eclipse.lsp4j.Position

class CompletionItemResolveCommandTestCase : CompletionItemResolveCommandTestBase() {
    fun `test resolves autoimport directive`() =
        checkHasAdditionalEdit(Position(55, 11), "java.util.HashMap", "dood")
}
