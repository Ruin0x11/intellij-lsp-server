package com.ruin.lsp.commands.document.completion

import com.android.ide.common.res2.DataFile
import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.KOTLIN_PROJECT
import com.ruin.lsp.forKotlin
import org.jetbrains.kotlin.idea.KotlinFileType

class CompletionCommandTestCaseKt : CompletionCommandTestBase() {
    override val projectName: String
        get() = KOTLIN_PROJECT

    override val filePath: String
        get() = forKotlin(DUMMY_FILE_PATH)

    override val fileType = KotlinFileType.INSTANCE!!

    fun `test function completion`() =
        doTest("functions.kt", "fun boring() {}", "bo<caret>",
            "boring() : Unit", "boring()")

    fun `test function completion with parameter`() =
        doTest("functions.kt","pub fun lessBoring(number: Int) : String = arg.toString()", "le<caret>",
            "lessBoring(number: Int) : String", "lessBoring(")

    fun `test function snippet completion`() =
        doTestWithSnippet("functions.kt","fun lessBoring(number: Int) : String = arg.toString()", "le<caret>",
            "lessBoring(number: Int) : String", "lessBoring(${'$'}${'{'}1:number${'}'})${'$'}0")

    fun `test variable completion`() =
        doTest("variables.kt", "pub val string = \"asdf\"", "st<caret>",
            "string : String", "string")

    fun `test class completion`() =
        doTest("MyClass.kt", "class MyClass {}", "MyC<caret>",
            "MyClass", "MyClass")

    fun `test value parameter completion`() =
        doTest("util.kt", "", "fun funcWithArg(number: Int) { num<caret> }",
            "number : Int", "number")

    fun `test java getter to property completion`() =
        doTest("MyClass.java", """
            |public class MyClass {
            |    private int value;
            |    public int getValue() { return this.value; }
            |    public void setValue(int value) { this.value = value; }
            |}
            """.trimMargin(), "MyClass().v<caret>",
            "value (from getValue()) : int", "value")

    fun `test java value to property completion`() =
        doTest("MyClass.java", """
            |public class MyClass {
            |    public int getThing() { return 42; }
            |}
            """.trimMargin(), "MyClass().t<caret>",
            "thing (from getThing()) : int", "thing")

    fun `test keyword name escaping`() =
        doTest("MyClass.java", """
            |public class MyClass {
            |    private Object object;
            |    public Object getObject() { return this.object; }
            |}
            """.trimMargin(), "MyClass().o<caret>",
            "object (from getObject()) : Object", "`object`")

    fun `test object`() =
        doTest("MyObject.kt", "object MyObject {}", "MyO<caret>",
            "MyObject : object", "MyObject")

    fun `test companion object`() =
        doTest("HasCompanion.kt", "class HasCompanion { companion object {} }", "HasCompanion.C<caret>",
            "Companion : object", "Companion")

    fun `test return statement in closure`() =
        doTest("util.kt", "", "listOf<Int>().forEach { retur<caret> }",
            "return@forEach", "return@forEach")

    fun `test closure with brace syntax`() =
        doTest("functions.kt", "fun withClosure(closure: (counter: Int, Int) -> Unit) {}", "withC<caret>",
            "withClosure { counter, Int -> ... } : Unit",
            "withClosure { counter, i -> ")

    fun `test closure with brace syntax and snippet`() =
        doTestWithSnippet("functions.kt", "fun withClosure(closure: (counter: Int, Int) -> Unit) {}", "withC<caret>",
            "withClosure { counter, Int -> ... } : Unit",
            "withClosure { ${'$'}${'{'}1:counter${'}'}, ${'$'}${'{'}2:i${'}'} -> ${'$'}0 }")

    fun `test closure with preceeding argument and brace syntax`() =
        doTest("functions.kt", "fun withClosureAndArg(str: String, closure: (counter: Int, Int) -> Unit) {}", "withC<caret>",
            "withClosureAndArg(str: String) { counter, Int -> ... } : Unit",
            "withClosureAndArg(")

    fun `test closure with preceeding argument, brace syntax and snippet`() =
        doTestWithSnippet("functions.kt", "fun withClosureAndArg(str: String, closure: (counter: Int, Int) -> Unit) {}", "withC<caret>",
            "withClosureAndArg(str: String) { counter, Int -> ... } : Unit",
            "withClosureAndArg(${'$'}${'{'}1:str${'}'}) { ${'$'}${'{'}2:counter${'}'}, ${'$'}${'{'}3:i${'}'} -> ${'$'}0 }")
}
