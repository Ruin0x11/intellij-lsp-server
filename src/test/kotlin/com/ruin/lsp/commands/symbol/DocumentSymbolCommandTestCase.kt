package com.ruin.lsp.commands.symbol

import com.ruin.lsp.*
import com.ruin.lsp.values.SymbolKind

class DocumentSymbolCommandTestCase : DocumentSymbolCommandTestBase() {

    fun `test file symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("Dummy.java", null, SymbolKind.FILE, range(0, 0, 54, 0))
    }

    fun `test module symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("java.util.ArrayList", "Dummy.java", SymbolKind.MODULE, range(2, 0, 2, 27))
    }

    fun `test package symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("org.lsp.javaproject", "Dummy.java", SymbolKind.PACKAGE, range(0, 0, 0, 28))
    }

    fun `test class symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("Dummy", "Dummy.java", SymbolKind.CLASS, range(8, 13, 8, 18))
    }

    fun `test method symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("boring()", "Dummy", SymbolKind.METHOD, range(10, 16, 10, 22))
            .assertHasSymbol("notBoring(int)", "Dummy", SymbolKind.METHOD, range(21, 16, 21, 25))
            .assertHasSymbol("notBoring(int, String)", "Dummy", SymbolKind.METHOD, range(25, 9, 25, 18))
            .assertHasSymbol("fluid()", "Dummy", SymbolKind.METHOD, range(27, 17, 27, 22))
            .assertHasSymbol("moreBoring()", "Dummy", SymbolKind.METHOD, range(44, 9, 44, 19))
            .assertHasSymbol("answerQuestion(String)", "Dummy", SymbolKind.METHOD, range(49, 15, 49, 29))
    }

    fun `test property symbol`() {
        getSymbols(SUBCLASS_FILE_PATH)
            .assertHasSymbol("@Override", "abstractMethod()", SymbolKind.PROPERTY, range(11, 4, 11, 13))
    }

    fun `test field symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("thingy", "Dummy", SymbolKind.FIELD, range(9, 16, 9, 22))
    }

    fun `test constructor symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("Dummy()", "Dummy", SymbolKind.CONSTRUCTOR, range(33, 4, 33, 9))
            .assertHasSymbol("Dummy(int)", "Dummy", SymbolKind.CONSTRUCTOR, range(35, 4, 35, 9))
            .assertHasSymbol("Dummy(String)", "Dummy", SymbolKind.CONSTRUCTOR, range(37, 4, 37, 9))
            .assertHasSymbol("Dummy(int, String)", "Dummy", SymbolKind.CONSTRUCTOR, range(39, 4, 39, 9))
    }

    fun `test enum symbol`() {
        getSymbols(ENUM_TYPE_FILE_PATH)
            .assertHasSymbol("EnumType", "EnumType.java", SymbolKind.ENUM, range(2, 12, 2, 20))
    }

    fun `test interface symbol`() {
        getSymbols(INTERFACE_FILE_PATH)
            .assertHasSymbol("MyInterface", "MyInterface.java", SymbolKind.INTERFACE, range(5, 17, 5, 28))
    }

    fun `test variable symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("list", "boring()", SymbolKind.VARIABLE, range(14, 26, 14, 30))
            .assertHasSymbol("number", "notBoring(int)", SymbolKind.VARIABLE, range(21, 30, 21, 36))
            .assertHasSymbol("problem", "notBoring(int)", SymbolKind.VARIABLE, range(22, 20, 22, 27))
            .assertHasSymbol("number", "notBoring(int, String)", SymbolKind.VARIABLE, range(25, 23, 25, 29))
            .assertHasSymbol("foo", "notBoring(int, String)", SymbolKind.VARIABLE, range(25, 38, 25, 41))
            .assertHasSymbol("number", "Dummy(int)", SymbolKind.VARIABLE, range(35, 14, 35, 20))
            .assertHasSymbol("string", "Dummy(String)", SymbolKind.VARIABLE, range(37, 17, 37, 23))
            .assertHasSymbol("number", "Dummy(int, String)", SymbolKind.VARIABLE, range(39, 14, 39, 20))
            .assertHasSymbol("andString", "Dummy(int, String)", SymbolKind.VARIABLE, range(39, 29, 39, 38))
            .assertHasSymbol("question", "answerQuestion(String)", SymbolKind.VARIABLE, range(49, 37, 49, 45))
    }

    fun `test constant symbol`() {
        getSymbols(CONSTANTS_FILE_PATH)
            .assertHasSymbol("INT", "Constants", SymbolKind.CONSTANT, range(3, 28, 3, 31))
            .assertHasSymbol("STRING", "Constants", SymbolKind.CONSTANT, range(4, 32, 4, 38))
            .assertHasSymbol("ENUM_TYPE", "Constants", SymbolKind.CONSTANT, range(5, 26, 5, 35))
    }

    fun `test string symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("\"Hi\"", "boring()", SymbolKind.STRING, range(11, 27, 11, 31))
            .assertHasSymbol("\"hi\"", "boring()", SymbolKind.STRING, range(15, 17, 15, 21))
            .assertHasSymbol("\"foo\"", "moreBoring()", SymbolKind.STRING, range(45, 22, 45, 27))
            .assertHasSymbol("\"life\"", "moreBoring()", SymbolKind.STRING, range(46, 33, 46, 39))
            .assertHasSymbol("\"universe\"", "moreBoring()", SymbolKind.STRING, range(46, 42, 46, 52))
    }

    fun `test number symbol`() {
        getSymbols(DUMMY_FILE_PATH)
            .assertHasSymbol("42", "thingy", SymbolKind.NUMBER, range(9, 25, 9, 27))
            .assertHasSymbol("42", "boring()", SymbolKind.NUMBER, range(16, 18, 16, 20))
            .assertHasSymbol("12", "boring()", SymbolKind.NUMBER, range(17, 17, 17, 19))
            .assertHasSymbol("42", "moreBoring()", SymbolKind.NUMBER, range(45, 18, 45, 20))
            .assertHasSymbol("42", "answerQuestion(String)", SymbolKind.NUMBER, range(50, 15, 50, 17))
    }

    fun `test boolean symbol`() {
        getSymbols(CONSTANTS_FILE_PATH)
            .assertHasSymbol("true", "yes", SymbolKind.BOOLEAN, range(8, 18, 8, 22))
            .assertHasSymbol("false", "no", SymbolKind.BOOLEAN, range(9, 17, 9, 22))
    }

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
}
