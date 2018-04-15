package com.ruin.lsp.commands.project.symbol

import com.ruin.lsp.*
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.SymbolInformation
import org.eclipse.lsp4j.SymbolKind

class WorkspaceSymbolCommandTestCaseKt : WorkspaceSymbolCommandTestBase() {
    override val projectName: String
        get() = KOTLIN_PROJECT

    fun `test finds function in companion object`() = checkHasResult("answerQuestion", listOf(
        SymbolInformation("answerQuestion(String)", SymbolKind.Function,
            Location(uriForPath(projectName, forKotlin(DUMMY_FILE_PATH)), range(47, 21, 47, 35))
        )
    ))

    fun `test finds method`() = checkHasResult("fluid", listOf(
        SymbolInformation("fluid()", SymbolKind.Method,
            Location(uriForPath(projectName, forKotlin(DUMMY_FILE_PATH)), range(27, 8, 27, 13)),
            "Dummy"
        )
    ))

    fun `test finds variable`() = checkHasResult("thingy", listOf(
        SymbolInformation("thingy", SymbolKind.Field,
            Location(uriForPath(projectName, forKotlin(DUMMY_FILE_PATH)), range(9, 16, 9, 22)),
            "Dummy"
        )
    ))

    fun `test finds class`() = checkHasResult("Dummy", listOf(
        SymbolInformation("Dummy", SymbolKind.Class,
            Location(uriForPath(projectName, forKotlin(DUMMY_FILE_PATH)), range(8, 11, 8, 16))
        )
    ))

    fun `test finds interface`() = checkHasResult("MyInterface", listOf(
        SymbolInformation("MyInterface", SymbolKind.Interface,
            Location(uriForPath(projectName, forKotlin(INTERFACE_FILE_PATH)), range(4, 10, 4, 21))
        )
    ))

    fun `test finds constant`()  = checkHasResult("STRING", listOf(
        SymbolInformation("STRING", SymbolKind.Constant,
            Location(uriForPath(projectName, forKotlin(CONSTANTS_FILE_PATH)), range(10, 26, 10, 32)),
            "Constants"
        )
    ))

    fun `test finds function outside package`()  = checkHasResult("outsidePackage", listOf(
        SymbolInformation("outsidePackage(int)", SymbolKind.Function,
            Location(uriForPath(projectName, forKotlin(DUMMY_FILE_PATH)), range(65, 4, 65, 18)),
            "DummyKt"
        )
    ))
}
