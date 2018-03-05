package com.ruin.lsp.commands.symbol

import com.ruin.lsp.*
import org.eclipse.lsp4j.SymbolKind

class DocumentSymbolCommandTestCase : DocumentSymbolCommandTestBase() {

    fun `test file symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("Dummy.java", null, SymbolKind.File, range(0, 0, 54, 0))
    }

    fun `test module symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("java.util.ArrayList", "Dummy.java", SymbolKind.Module, range(2, 0, 2, 27))
    }

    fun `test package symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("org.lsp.javaproject", "Dummy.java", SymbolKind.Package, range(0, 0, 0, 28))
    }

    fun `test class symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("Dummy", "Dummy.java", SymbolKind.Class, range(8, 13, 8, 18))
    }

    fun `test method symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("boring()", "Dummy", SymbolKind.Method, range(10, 16, 10, 22))
            .assertHasSymbol("notBoring(int)", "Dummy", SymbolKind.Method, range(21, 16, 21, 25))
            .assertHasSymbol("notBoring(int, String)", "Dummy", SymbolKind.Method, range(25, 9, 25, 18))
            .assertHasSymbol("fluid()", "Dummy", SymbolKind.Method, range(27, 17, 27, 22))
            .assertHasSymbol("moreBoring()", "Dummy", SymbolKind.Method, range(44, 9, 44, 19))
            .assertHasSymbol("answerQuestion(String)", "Dummy", SymbolKind.Method, range(49, 15, 49, 29))
    }

    fun `test property symbol`() {
        getSymbols(SUBCLASS_FILE_PATH)
            .assertHasSymbol("@Override", "abstractMethod()", SymbolKind.Property, range(11, 4, 11, 13))
    }

    fun `test field symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("thingy", "Dummy", SymbolKind.Field, range(9, 16, 9, 22))
    }

    fun `test constructor symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("Dummy()", "Dummy", SymbolKind.Constructor, range(33, 4, 33, 9))
            .assertHasSymbol("Dummy(int)", "Dummy", SymbolKind.Constructor, range(35, 4, 35, 9))
            .assertHasSymbol("Dummy(String)", "Dummy", SymbolKind.Constructor, range(37, 4, 37, 9))
            .assertHasSymbol("Dummy(int, String)", "Dummy", SymbolKind.Constructor, range(39, 4, 39, 9))
    }

    fun `test enum symbol`() {
        getSymbols(ENUM_TYPE_FILE_PATH)
            .assertHasSymbol("EnumType", "EnumType.java", SymbolKind.Enum, range(2, 12, 2, 20))
    }

    fun `test interface symbol`() {
        getSymbols(INTERFACE_FILE_PATH)
            .assertHasSymbol("MyInterface", "MyInterface.java", SymbolKind.Interface, range(5, 17, 5, 28))
    }

    fun `test variable symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("list", "boring()", SymbolKind.Variable, range(14, 26, 14, 30))
            .assertHasSymbol("number", "notBoring(int)", SymbolKind.Variable, range(21, 30, 21, 36))
            .assertHasSymbol("problem", "notBoring(int)", SymbolKind.Variable, range(22, 20, 22, 27))
            .assertHasSymbol("number", "notBoring(int, String)", SymbolKind.Variable, range(25, 23, 25, 29))
            .assertHasSymbol("foo", "notBoring(int, String)", SymbolKind.Variable, range(25, 38, 25, 41))
            .assertHasSymbol("number", "Dummy(int)", SymbolKind.Variable, range(35, 14, 35, 20))
            .assertHasSymbol("string", "Dummy(String)", SymbolKind.Variable, range(37, 17, 37, 23))
            .assertHasSymbol("number", "Dummy(int, String)", SymbolKind.Variable, range(39, 14, 39, 20))
            .assertHasSymbol("andString", "Dummy(int, String)", SymbolKind.Variable, range(39, 29, 39, 38))
            .assertHasSymbol("question", "answerQuestion(String)", SymbolKind.Variable, range(49, 37, 49, 45))
    }

    fun `test constant symbol`() {
        getSymbols(CONSTANTS_FILE_PATH)
            .assertHasSymbol("INT", "Constants", SymbolKind.Constant, range(3, 28, 3, 31))
            .assertHasSymbol("STRING", "Constants", SymbolKind.Constant, range(4, 32, 4, 38))
            .assertHasSymbol("ENUM_TYPE", "Constants", SymbolKind.Constant, range(5, 26, 5, 35))
    }

    fun `test string symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("\"Hi\"", "boring()", SymbolKind.String, range(11, 27, 11, 31))
            .assertHasSymbol("\"hi\"", "boring()", SymbolKind.String, range(15, 17, 15, 21))
            .assertHasSymbol("\"foo\"", "moreBoring()", SymbolKind.String, range(45, 22, 45, 27))
            .assertHasSymbol("\"life\"", "moreBoring()", SymbolKind.String, range(46, 33, 46, 39))
            .assertHasSymbol("\"universe\"", "moreBoring()", SymbolKind.String, range(46, 42, 46, 52))
    }

    fun `test number symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("42", "thingy", SymbolKind.Number, range(9, 25, 9, 27))
            .assertHasSymbol("42", "boring()", SymbolKind.Number, range(16, 18, 16, 20))
            .assertHasSymbol("12", "boring()", SymbolKind.Number, range(17, 17, 17, 19))
            .assertHasSymbol("42", "moreBoring()", SymbolKind.Number, range(45, 18, 45, 20))
            .assertHasSymbol("42", "answerQuestion(String)", SymbolKind.Number, range(50, 15, 50, 17))
    }

    fun `test boolean symbol`() {
        getSymbols(CONSTANTS_FILE_PATH)
            .assertHasSymbol("true", "yes", SymbolKind.Boolean, range(8, 18, 8, 22))
            .assertHasSymbol("false", "no", SymbolKind.Boolean, range(9, 17, 9, 22))
    }

    // TODO: readd when supported by lsp4j
    /*
    fun `test null symbol`() {
        getSymbols(CONSTANTS_FILE_PATH)
            .assertHasSymbol("null", "maybe", SymbolKind.NULL, range(10, 20, 10, 24))
    }

    fun `test enum member symbol`() {
        getSymbols(ENUM_TYPE_FILE_PATH)
            .assertHasSymbol("FOO", "EnumType", SymbolKind.ENUM_MEMBER, range(3, 4, 3, 7))
            .assertHasSymbol("BAR", "EnumType", SymbolKind.ENUM_MEMBER, range(4, 4, 4, 7))
            .assertHasSymbol("BAZ", "EnumType", SymbolKind.ENUM_MEMBER, range(5, 4, 5, 7))
    }
    */
}
