package com.ruin.lsp.commands.project.symbol

import com.ruin.lsp.*
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.SymbolInformation
import org.eclipse.lsp4j.SymbolKind

class WorkspaceSymbolCommandTestCase : WorkspaceSymbolCommandTestBase() {
    override val projectName: String
        get() = JAVA_PROJECT

    fun `test finds method`() = checkHasResult("answerQuestion", listOf(
        SymbolInformation("answerQuestion(String)", SymbolKind.Method,
            Location(uriForPath(projectName, DUMMY_FILE_PATH), range(49, 15, 49, 29)),
            "Dummy"
        )
    ))

    fun `test finds variable`() = checkHasResult("thingy", listOf(
        SymbolInformation("thingy", SymbolKind.Field,
            Location(uriForPath(projectName, DUMMY_FILE_PATH), range(9, 16, 9, 22)),
            "Dummy"
        )
    ))

    fun `test finds class`() = checkHasResult("Dummy", listOf(
        SymbolInformation("Dummy", SymbolKind.Class,
            Location(uriForPath(projectName, DUMMY_FILE_PATH), range(8, 13, 8, 18))
        )
    ))

    fun `test finds interface`() = checkHasResult("MyInterface", listOf(
        SymbolInformation("MyInterface", SymbolKind.Interface,
            Location(uriForPath(projectName, INTERFACE_FILE_PATH), range(5, 17, 5, 28))
        )
    ))

    fun `test finds constant`()  = checkHasResult("STRING", listOf(
        SymbolInformation("STRING", SymbolKind.Constant,
            Location(uriForPath(projectName, CONSTANTS_FILE_PATH), range(4, 32, 4, 38)),
            "Constants"
        )
    ))
}
