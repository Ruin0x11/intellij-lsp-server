package com.ruin.lsp.commands.document.completion

import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.JAVA_PROJECT
import org.eclipse.lsp4j.Position

class CompletionCommandTestCase : CompletionCommandTestBase() {
    override val projectName: String
        get() = JAVA_PROJECT

    override val filePath: String
        get() = DUMMY_FILE_PATH

    fun `test function completion`() =
        checkContainsCompletion(Position(13, 29), false, "boring() : void", "boring()")

    fun `test function completion with parameter`() =
        checkContainsCompletion(Position(46, 32), false, "answerQuestion(String question) : int", "answerQuestion(")

    fun `test function snippet`() =
        checkContainsCompletion(Position(13, 29), true, "notBoring(int number) : void", "notBoring(${'$'}${'{'}1:number${'}'})${'$'}0")

    fun `test variable completion`() =
        checkContainsCompletion(Position(15, 12), false,"list : ArrayList<String>", "list")

    fun `test class completion`() =
        checkContainsCompletion(Position(13, 17), false,"org.lsp.javaproject.Dummy", "Dummy")

}
