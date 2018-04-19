package com.ruin.lsp.commands.document.completion

import com.android.ide.common.res2.DataFile
import com.intellij.openapi.editor.LogicalPosition
import com.intellij.openapi.fileTypes.FileType
import com.intellij.psi.PsiFile
import com.ruin.lsp.model.invokeCommandAndWait
import org.eclipse.lsp4j.InsertTextFormat
import org.eclipse.lsp4j.Position
import org.intellivim.FileEditingTestCase
import org.jetbrains.kotlin.idea.KotlinFileType

abstract class CompletionCommandTestBase : FileEditingTestCase() {
    abstract val fileType: FileType

    protected fun checkContainsCompletion(pos: Position, snippet: Boolean, expected: String, expectedInsert: String) {
        val command = CompletionCommand(pos, snippet)
        val result = invokeCommandAndWait(command, project, psiFile)
        assertTrue(result.right.items.all {
            it.insertTextFormat == InsertTextFormat.PlainText ||
                it.insertTextFormat == InsertTextFormat.Snippet
        })
        assertTrue("Expected $expected, $expectedInsert to be included but got: \n${result.right}",
            result.right.items.any { it.label == expected && it.insertText == expectedInsert })
    }

    protected fun doTest(name: String, code: String, editLine: String, expected: String, expectedInsert: String) =
        run(name, code, editLine, expected, expectedInsert, false)

    protected fun doTestWithSnippet(name: String, code: String, editLine: String, expected: String, expectedInsert: String) =
        run(name, code, editLine, expected, expectedInsert, true)

    private fun run(name: String, code: String, editLine: String, expected: String, expectedInsert: String, snippet: Boolean) {
        val (file, pos) = if (fileType == KotlinFileType.INSTANCE) {
            setupKotlin(name, code, editLine)
        } else {
            setupJava(name, code, editLine)
        }

        assert(pos != Position(0, 0))

        val command = CompletionCommand(pos, snippet)
        val result = invokeCommandAndWait(command, myFixture.project, file)
        assertTrue(result.right.items.all {
            it.insertTextFormat == if (snippet) InsertTextFormat.Snippet else InsertTextFormat.PlainText
        })
        assertTrue("Expected $expected, $expectedInsert to be included but got: \n${result.right}",
            result.right.items.any { it.label == expected && it.insertText == expectedInsert })
    }

    private fun setupJava(name: String, code: String, editLine: String): Pair<PsiFile, Position> {
        return setup(name, code, "Test.java",
            """public class Test {
                public void test() {
                    $editLine
                }
            }""".trimIndent())
    }

    private fun setupKotlin(name: String, code: String, editLine: String): Pair<PsiFile, Position> {
        return setup(name, code, "Test.kt",
            """pub class Test {
                fun test() {
                    $editLine
                }
            }""".trimIndent())
    }

    private fun setup(name: String, code: String, editorName: String, editorCode: String): Pair<PsiFile, Position> {
        myFixture.addFileToProject("./$name", code)
        val file = myFixture.configureByText(editorName, editorCode)
        val pos = myFixture.editor.caretModel.logicalPosition.position()
        return Pair(file, pos)
    }

    fun LogicalPosition.position() = Position(this.line, this.column)
}
