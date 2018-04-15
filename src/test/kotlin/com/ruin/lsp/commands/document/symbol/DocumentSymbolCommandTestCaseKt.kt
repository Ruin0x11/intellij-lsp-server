package com.ruin.lsp.commands.document.symbol

import com.ruin.lsp.*
import org.eclipse.lsp4j.SymbolKind

class DocumentSymbolCommandTestCaseKt : DocumentSymbolCommandTestBase() {
    override val projectName = KOTLIN_PROJECT

    fun `test file symbol`() {
        getSymbols(forKotlin(DUMMY_FILE_PATH))
            .assertHasSymbol("Dummy.kt", null, SymbolKind.File, range(0, 0, 60, 0))
    }

    fun `test module symbol`() {
        getSymbols(forKotlin(DUMMY_FILE_PATH))
            .assertHasSymbol("org.lsp.kotlinproject.Dummy", "Dummy.kt", SymbolKind.Module, range(2, 0, 2, 34))
    }

    fun `test package symbol`() {
        getSymbols(forKotlin(DUMMY_FILE_PATH))
            .assertHasSymbol("org.lsp.kotlinproject", "Dummy.kt", SymbolKind.Package, range(0, 0, 0, 29))
    }

    fun `test class symbol`() {
        getSymbols(forKotlin(DUMMY_FILE_PATH))
            .assertHasSymbol("Dummy", "Dummy.kt", SymbolKind.Class, range(8, 11, 8, 16))
    }

    fun `test method symbol`() {
        getSymbols(forKotlin(DUMMY_FILE_PATH))
            .assertHasSymbol("boring()", "Dummy", SymbolKind.Method, range(10, 8, 10, 14))
            .assertHasSymbol("notBoring(Int)", "Dummy", SymbolKind.Method, range(21, 8, 21, 17))
            .assertHasSymbol("notBoring(Int, String)", "Dummy", SymbolKind.Method, range(25, 17, 25, 26))
            .assertHasSymbol("fluid()", "Dummy", SymbolKind.Method, range(27, 8, 27, 13))
            .assertHasSymbol("moreBoring()", "Dummy", SymbolKind.Method, range(40, 17, 40, 27))
            .assertHasSymbol("answerQuestion(String)", "Dummy", SymbolKind.Method, range(47, 21, 47, 35))
    }

    fun `test property symbol`() {
        getSymbols(forKotlin(SUBCLASS_FILE_PATH))
            .assertHasSymbol("@Override", "abstractMethod()", SymbolKind.Property, range(10, 4, 10, 13))
    }

    fun `test field symbol`() {
        getSymbols(forKotlin(DUMMY_FILE_PATH))
            .assertHasSymbol("thingy", "Dummy", SymbolKind.Field, range(9, 16, 9, 22))
    }

    fun `test constructor symbol`() {
        getSymbols(forKotlin(DUMMY_FILE_PATH))
            .assertHasSymbol("Dummy()", "Dummy", SymbolKind.Constructor, range(33, 4, 33, 29))
            .assertHasSymbol("Dummy(Int)", "Dummy", SymbolKind.Constructor, range(34, 4, 34, 40))
            .assertHasSymbol("Dummy(String)", "Dummy", SymbolKind.Constructor, range(35, 4, 35, 43))
            .assertHasSymbol("Dummy(Int, String)", "Dummy", SymbolKind.Constructor, range(36, 4, 36, 59))
    }

    fun `test enum symbol`() {
        getSymbols(forKotlin(ENUM_TYPE_FILE_PATH))
            .assertHasSymbol("EnumType", "EnumType.kt", SymbolKind.Enum, range(2, 11, 2, 19))
    }

    fun `test interface symbol`() {
        getSymbols(forKotlin(INTERFACE_FILE_PATH))
            .assertHasSymbol("MyInterface", "MyInterface.kt", SymbolKind.Interface, range(4, 10, 4, 21))
    }

    fun `test variable symbol`() {
        getSymbols(forKotlin(DUMMY_FILE_PATH))
            .assertHasSymbol("list", "boring()", SymbolKind.Variable, range(14, 12, 14, 16))
            .assertHasSymbol("number", "notBoring(Int)", SymbolKind.Variable, range(21, 18, 21, 24))
            .assertHasSymbol("problem", "notBoring(Int)", SymbolKind.Variable, range(22, 12, 22, 19))
            .assertHasSymbol("number", "notBoring(Int, String)", SymbolKind.Variable, range(25, 27, 25, 33))
            .assertHasSymbol("foo", "notBoring(Int, String)", SymbolKind.Variable, range(25, 40, 25, 43))
            .assertHasSymbol("number", "Dummy(Int)", SymbolKind.Variable, range(34, 25, 34, 31))
            .assertHasSymbol("string", "Dummy(String)", SymbolKind.Variable, range(35, 25, 35, 31))
            .assertHasSymbol("number", "Dummy(Int, String)", SymbolKind.Variable, range(36, 25, 36, 31))
            .assertHasSymbol("andString", "Dummy(Int, String)", SymbolKind.Variable, range(36, 38, 36, 47))
            .assertHasSymbol("question", "answerQuestion(String)", SymbolKind.Variable, range(47, 36, 47, 44))
    }

    fun `test constant symbol`() {
        getSymbols(forKotlin(CONSTANTS_FILE_PATH))
            .assertHasSymbol("INT", "Constants", SymbolKind.Constant, range(9, 18, 9, 21))
            .assertHasSymbol("STRING", "Constants", SymbolKind.Constant, range(10, 26, 10, 32))
            .assertHasSymbol("ENUM_TYPE", "Constants", SymbolKind.Constant, range(11, 18, 11, 27))
    }

    fun `test string symbol`() {
        getSymbols(forKotlin(DUMMY_FILE_PATH))
            .assertHasSymbol("\"Hi\"", "boring()", SymbolKind.String, range(11, 16, 11, 20))
            .assertHasSymbol("\"hi\"", "boring()", SymbolKind.String, range(15, 17, 15, 21))
            .assertHasSymbol("\"foo\"", "moreBoring()", SymbolKind.String, range(41, 22, 41, 27))
            .assertHasSymbol("\"life\"", "moreBoring()", SymbolKind.String, range(42, 33, 42, 39))
            .assertHasSymbol("\"universe\"", "moreBoring()", SymbolKind.String, range(42, 42, 42, 52))
    }

    fun `test number symbol`() {
        getSymbols(forKotlin(DUMMY_FILE_PATH))
            .assertHasSymbol("42", "thingy", SymbolKind.Number, range(9, 25, 9, 27))
            .assertHasSymbol("42", "boring()", SymbolKind.Number, range(16, 18, 16, 20))
            .assertHasSymbol("12", "boring()", SymbolKind.Number, range(17, 17, 17, 19))
            .assertHasSymbol("42", "moreBoring()", SymbolKind.Number, range(41, 18, 41, 20))
            .assertHasSymbol("42", "answerQuestion(String)", SymbolKind.Number, range(48, 19, 48, 21))
    }

    fun `test boolean symbol`() {
        getSymbols(forKotlin(CONSTANTS_FILE_PATH))
            .assertHasSymbol("true", "yes", SymbolKind.Boolean, range(4, 14, 4, 18))
            .assertHasSymbol("false", "no", SymbolKind.Boolean, range(5, 13, 5, 18))
    }

    // TODO: readd when supported by lsp4j
    /*
    fun `test null symbol`() {
        getSymbols(forKotlin(CONSTANTS_FILE_PATH))
            .assertHasSymbol("null", "maybe", SymbolKind.NULL, range(10, 20, 10, 24))
    }

    fun `test enum member symbol`() {
        getSymbols(forKotlin(ENUM_TYPE_FILE_PATH))
            .assertHasSymbol("FOO", "EnumType", SymbolKind.ENUM_MEMBER, range(3, 4, 3, 7))
            .assertHasSymbol("BAR", "EnumType", SymbolKind.ENUM_MEMBER, range(4, 4, 4, 7))
            .assertHasSymbol("BAZ", "EnumType", SymbolKind.ENUM_MEMBER, range(5, 4, 5, 7))
    }
    */
}
