package com.ruin.lsp.commands.document.hover

import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.KOTLIN_PROJECT
import com.ruin.lsp.forKotlin
import org.eclipse.lsp4j.MarkedString

class HoverCommandTestCaseKt : HoverCommandTestBase() {
    override val projectName: String
        get() = KOTLIN_PROJECT

    override val filePath: String
        get() = forKotlin(DUMMY_FILE_PATH)

    fun `test hover finds class info`() = checkHoverEquals(8, 16,
        MarkedString("kotlin", "public open class Dummy defined in org.lsp.kotlinproject in file Dummy.kt"))

    fun `test hover finds method info`() = checkHoverEquals(42, 25,
        MarkedString("kotlin", "internal final fun answerQuestion(question: String): Int defined in org.lsp.kotlinproject.Dummy.Companion"))

    fun `test hover finds field info`() = checkHoverEquals(17, 11,
        MarkedString("kotlin", "private final var thingy: Int defined in org.lsp.kotlinproject.Dummy"))

    fun `test hover finds variable info`() = checkHoverEquals(15, 10,
        MarkedString("kotlin", "val list: [ERROR : Type for List()]"))

    fun `test hover finds closure in function value parameters with arrow`() = checkHoverEquals(59, 27,
        MarkedString("kotlin", "internal final fun withClosure(closure: (counter: Int, Int) → Unit): Unit defined in org.lsp.kotlinproject.Dummy.Companion"))

    fun `test hover finds closure parameter`() = checkHoverEquals(60, 19,
        MarkedString("kotlin", "value-parameter closure: (counter: Int, Int) → Unit"))

    fun `test hover finds java information in kotlin file`() = checkHoverEquals(95, 19,
        MarkedString("java", "package org.lsp.kotlinproject; public class GetSet extends java.lang.Object"))

    fun `test hover finds nothing`() = checkHoverIsEmpty(32, 1)
}
