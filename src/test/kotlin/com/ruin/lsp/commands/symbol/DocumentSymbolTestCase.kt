package com.ruin.lsp.commands.symbol

import com.ruin.lsp.DATA_FILE_PATH
import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.INTERFACE_FILE_PATH
import com.ruin.lsp.values.SymbolKind

class DocumentSymbolTestCase : DocumentSymbolTestBase() {
    fun `test finds variable`() = checkFindsSymbol(DUMMY_FILE_PATH,
        "list", SymbolKind.VARIABLE)

    fun `test finds field`() = checkFindsSymbol(DUMMY_FILE_PATH,
        "thingy", SymbolKind.FIELD)

    fun `test finds package`() = checkFindsSymbol(DUMMY_FILE_PATH,
        "import java.util.ArrayList", SymbolKind.PACKAGE)

    fun `test finds class`() = checkFindsSymbol(DUMMY_FILE_PATH,
        "Dummy", SymbolKind.CLASS)

    fun `test finds method`() = checkFindsSymbol(DUMMY_FILE_PATH,
        "notBoring", SymbolKind.METHOD)

    fun `test finds interface`() = checkFindsSymbol(INTERFACE_FILE_PATH,
        "MyInterface", SymbolKind.INTERFACE)

    fun `test finds enum`() = checkFindsSymbol(DATA_FILE_PATH,
        "MyEnum", SymbolKind.ENUM)

    fun `test finds enum member`() = checkFindsSymbol(DATA_FILE_PATH,
        "DOOD", SymbolKind.ENUM_MEMBER)

    fun `test finds constant`() = checkFindsSymbol(DATA_FILE_PATH,
        "CONSTANT", SymbolKind.CONSTANT)
}
