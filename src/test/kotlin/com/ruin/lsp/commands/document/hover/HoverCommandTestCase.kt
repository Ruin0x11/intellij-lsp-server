package com.ruin.lsp.commands.document.hover

class HoverCommandTestCase : HoverCommandTestBase() {
    fun `test hover finds class info`() = checkHoverEquals(8, 16,
        "package org.lsp.javaproject; public class Dummy extends java.lang.Object ")

    fun `test hover finds method info`() = checkHoverEquals(46, 25,
        "static int answerQuestion(String question)")

    fun `test hover finds field info`() = checkHoverEquals(17, 11,
        "private int thingy = 42")

    fun `test hover finds variable info`() = checkHoverEquals(15, 10,
        "ArrayList<String> list = new ArrayList<String>()")

    fun `test hover finds nothing`() = checkHoverIsEmpty(32, 1)
}
