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
        checkContainsCompletion(Position(61, 15), false,"closure : (Int) -> Unit", "closure")

    fun `test java getter to property completion`() =
        checkContainsCompletion(Position(71, 16), false,"value (from getValue()) : int", "value")

    fun `test keyword name escaping`() =
        checkContainsCompletion(Position(72, 16), false,"object (from getObject()) : Object", "`object`")

    fun `test size property from getSize`() =
        checkContainsCompletion(Position(77, 9), false,"size (from getSize()) : int", "size")

    fun `test forEach`() =
        checkContainsCompletion(Position(77, 9), false,"forEach", "forEach(")

    fun `test kotlin collection method`() =
        checkContainsCompletion(Position(77, 9), false,"map", "map(")

    fun `test closure with brace syntax`() =
        checkContainsCompletion(Position(77, 9), false,"map {", "map {")
}
