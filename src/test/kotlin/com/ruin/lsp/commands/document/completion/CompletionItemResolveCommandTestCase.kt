package com.ruin.lsp.commands.document.completion

import com.ruin.lsp.range
import org.eclipse.lsp4j.TextEdit

class CompletionItemResolveCommandTestCase : CompletionItemResolveCommandTestBase() {
    fun `test resolves autoimport directive`() {
        myFixture.addClass("package foo; public class NotImported {}")
        myFixture.configureByText("""Main.java""", """public class Main { Not<caret> }""")
        checkHasAdditionalEdits("foo.NotImported", listOf(
            TextEdit(range(0, 0, 0, 0), "import foo.NotImported;"))
        )
    }

    fun `test doesn't import twice`() {
        myFixture.addClass("package foo; public class AlreadyImported {}")
        myFixture.configureByText("""Main.java""", """
            import foo.AlreadyImported;
            public class Main { Already<caret> }
            """.trimIndent())
        checkHasAdditionalEdits("foo.AlreadyImported", listOf())
    }
}

