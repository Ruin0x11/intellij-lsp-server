package com.ruin.lsp.commands.document.hover

import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.JAVA_PROJECT
import org.eclipse.lsp4j.MarkedString

class HoverCommandTestCase : HoverCommandTestBase() {
    override val projectName: String
        get() = JAVA_PROJECT

    override val filePath: String
        get() = DUMMY_FILE_PATH

    fun `test hover finds class info`() = checkHoverEquals(8, 16,
        MarkedString("java", "package org.lsp.javaproject; public class Dummy extends java.lang.Object"))

    fun `test hover finds method info`() = checkHoverEquals(46, 25,
        MarkedString("java", "static int answerQuestion(String question)"))

    fun `test hover finds field info`() = checkHoverEquals(17, 11,
        MarkedString("java", "private int thingy = 42"))

    fun `test hover finds variable info`() = checkHoverEquals(15, 10,
        MarkedString("java", "ArrayList<String> list = new ArrayList<String>()"))

    fun `test hover finds nothing`() = checkHoverIsEmpty(32, 1)
}
