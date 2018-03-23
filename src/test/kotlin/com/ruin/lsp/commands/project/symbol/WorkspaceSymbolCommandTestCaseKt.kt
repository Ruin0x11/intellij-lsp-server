package com.ruin.lsp.commands.project.symbol

import com.ruin.lsp.*
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.SymbolInformation
import org.eclipse.lsp4j.SymbolKind

class WorkspaceSymbolCommandTestCaseKt : WorkspaceSymbolCommandTestBase() {
    override val projectName: String
        get() = KOTLIN_PROJECT

    fun `test finds static function`() = checkHasResult("answerQuestion", listOf(
        SymbolInformation("answerQuestion", SymbolKind.Function,
            Location(uriForPath(projectName, forKotlin(DUMMY_FILE_PATH)), range(47, 21, 47, 21))
        )
    ))

    fun `test finds method`() = checkHasResult("notBoring", listOf(
        SymbolInformation("notBoring", SymbolKind.Function,
            Location(uriForPath(projectName, forKotlin(DUMMY_FILE_PATH)), range(25, 17, 25, 17))
        )
    ))

    fun `test finds variable`() = checkHasResult("thingy", listOf(
        SymbolInformation("thingy", SymbolKind.Field,
            Location(uriForPath(projectName, forKotlin(DUMMY_FILE_PATH)), range(9, 16, 9, 16)),
            "Dummy"
        )
    ))

    fun `test finds class`() = checkHasResult("Dummy", listOf(
        SymbolInformation("Dummy", SymbolKind.Class,
            Location(uriForPath(projectName, forKotlin(DUMMY_FILE_PATH)), range(8, 11, 8, 11))
        )
    ))

    fun `test finds interface`() = checkHasResult("MyInterface", listOf(
        SymbolInformation("MyInterface", SymbolKind.Interface,
            Location(uriForPath(projectName, forKotlin(INTERFACE_FILE_PATH)), range(4, 10, 4, 10))
        )
    ))

    fun `test finds constant`()  = checkHasResult("STRING", listOf(
        SymbolInformation("STRING", SymbolKind.Constant,
            Location(uriForPath(projectName, forKotlin(CONSTANTS_FILE_PATH)), range(4, 32, 4, 32)),
            "Constants"
        )
    ))
}
