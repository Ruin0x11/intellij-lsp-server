package com.ruin.lsp.commands.project.symbol

import com.ruin.lsp.*
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.SymbolInformation
import org.eclipse.lsp4j.SymbolKind

class SymbolCommandTestCase : SymbolCommandTestBase() {
    fun `test finds method`() = checkHasResult("answerQuestion", listOf(
        SymbolInformation("answerQuestion", SymbolKind.Method,
            Location(uriForPath(projectName, DUMMY_FILE_PATH), range(49, 15, 49, 15)),
            "Dummy"
        )
    ))

    fun `test finds variable`() = checkHasResult("thingy", listOf(
        SymbolInformation("thingy", SymbolKind.Field,
            Location(uriForPath(projectName, DUMMY_FILE_PATH), range(9, 16, 9, 16)),
            "Dummy"
        )
    ))

    fun `test finds class`() = checkHasResult("Dummy", listOf(
        SymbolInformation("Dummy", SymbolKind.Class,
            Location(uriForPath(projectName, DUMMY_FILE_PATH), range(8, 13, 8, 13))
        )
    ))

    fun `test finds interface`() = checkHasResult("MyInterface", listOf(
        SymbolInformation("MyInterface", SymbolKind.Interface,
            Location(uriForPath(projectName, INTERFACE_FILE_PATH), range(5, 17, 5, 17))
        )
    ))

    fun `test finds constant`()  = checkHasResult("STRING", listOf(
        SymbolInformation("STRING", SymbolKind.Constant,
            Location(uriForPath(projectName, CONSTANTS_FILE_PATH), range(4, 32, 4, 32)),
            "Constants"
        )
    ))
}
