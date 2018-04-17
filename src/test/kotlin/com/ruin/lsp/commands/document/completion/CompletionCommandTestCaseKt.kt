package com.ruin.lsp.commands.document.completion

import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.KOTLIN_PROJECT
import com.ruin.lsp.forKotlin
import org.eclipse.lsp4j.Position

class CompletionCommandTestCaseKt : CompletionCommandTestBase() {
    override val projectName: String
        get() = KOTLIN_PROJECT

    override val filePath: String
        get() = forKotlin(DUMMY_FILE_PATH)

    fun `test function completion`() =
        checkContainsCompletion(Position(13, 30), false, "boring() : Unit", "boring()")

    fun `test function completion with parameter`() =
        checkContainsCompletion(Position(42, 32), false, "answerQuestion(question: String) : Int", "answerQuestion(")

    fun `test function snippet`() =
        checkContainsCompletion(Position(16, 17), true, "notBoring(number: Int) : Unit", "notBoring(${'$'}${'{'}1:number${'}'})${'$'}0")

    fun `test variable completion`() =
        checkContainsCompletion(Position(15, 12), false,"list : [ERROR : Type for List<String>()]", "list")

    fun `test class completion`() =
        checkContainsCompletion(Position(13, 13), false,"org.lsp.kotlinproject.Dummy", "Dummy")

    fun `test value parameter completion`() =
        checkContainsCompletion(Position(64, 23), false,"str : String", "str")

    fun `test java getter to property completion`() =
        checkContainsCompletion(Position(75, 16), false,"value (from getValue()) : int", "value")

    fun `test keyword name escaping`() =
        checkContainsCompletion(Position(76, 16), false,"object (from getObject()) : Object", "`object`")

    fun `test object`() =
        checkContainsCompletion(Position(84, 12), false,"MyObject : object", "MyObject")

    fun `test companion object`() =
        checkContainsCompletion(Position(85, 17), false,"Companion : object", "Companion")

    fun `test closure with brace syntax`() =
        checkContainsCompletion(Position(80, 11), false,
            "withClosure { counter, Int -> ... } : Unit",
            "withClosure { counter, i -> ")

    fun `test closure with brace syntax and snippet`() =
        checkContainsCompletion(Position(80, 11), true,
            "withClosure { counter, Int -> ... } : Unit",
            "withClosure { ${'$'}${'{'}1:counter${'}'}, ${'$'}${'{'}2:i${'}'} -> ${'$'}0 }")

    fun `test closure with preceeding argument and brace syntax`() =
        checkContainsCompletion(Position(80, 11), false,
            "withClosureAndArg(str: String) { counter, Int -> ... } : Unit",
            "withClosureAndArg(")

    fun `test closure with preceeding argument, brace syntax and snippet`() =
        checkContainsCompletion(Position(80, 11), true,
            "withClosureAndArg(str: String) { counter, Int -> ... } : Unit",
            "withClosureAndArg(${'$'}${'{'}1:str${'}'}) { ${'$'}${'{'}2:counter${'}'}, ${'$'}${'{'}3:i${'}'} -> ${'$'}0 }")
}
