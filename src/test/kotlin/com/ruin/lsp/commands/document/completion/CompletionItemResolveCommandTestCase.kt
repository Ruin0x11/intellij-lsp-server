package com.ruin.lsp.commands.document.completion

import com.ruin.lsp.range
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.TextEdit

class CompletionItemResolveCommandTestCase : CompletionItemResolveCommandTestBase() {
    fun `test resolves autoimport directive`() =
        checkHasAdditionalEdit(Position(55, 11),
            "java.util.LinkedHashMap",
            TextEdit(range(3, 0, 3, 0), "import java.util.LinkedHashMap;\n"))

    fun `test doesn't import twice`() =
        checkHasNoAdditionalEdits(Position(56, 10),
            "java.util.ArrayList")
}
