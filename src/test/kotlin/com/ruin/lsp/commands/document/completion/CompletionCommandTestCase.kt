package com.ruin.lsp.commands.document.completion

import com.intellij.ide.highlighter.JavaFileType
import com.intellij.openapi.fileTypes.FileType
import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.JAVA_PROJECT

class CompletionCommandTestCase : CompletionCommandTestBase() {
    override val projectName: String
        get() = JAVA_PROJECT

    override val filePath: String
        get() = DUMMY_FILE_PATH

    override val fileType = JavaFileType.INSTANCE!!

    fun `test function completion`() =
        doTest("MyClass.java", """
            |public class MyClass {
            |   public void myFunction() { }
            |}
            """.trimMargin(), "new MyClass().myF<caret>", "myFunction() : void", "myFunction()")

    fun `test function with parameter completion`() =
        doTest("MyClass.java", """
            |public class MyClass {
            |   public int myFunction(int number) { return 1; }
            |}
            """.trimMargin(), "new MyClass().myF<caret>", "myFunction(int number) : int", "myFunction(")

    fun `test function snippet completion`() =
        doTestWithSnippet("MyClass.java", """
            |public class MyClass {
            |   public int myFunction(int number) { return 1; }
            |}
            """.trimMargin(), "new MyClass().myF<caret>", "myFunction(int number) : int", "myFunction(${'$'}${'{'}1:number${'}'})${'$'}0")

    fun `test variable completion`() =
        doTest("MyClass.java", """
            |public class MyClass {
            |   public ArrayList<String> list;
            |}
            """.trimMargin(), "new MyClass().li<caret>", "list : ArrayList<String>", "list")

    fun `test class completion`() =
        doTest("MyClass.java", "public static class MyClass {}", "MyCl<caret>", "MyClass", "MyClass")
}
