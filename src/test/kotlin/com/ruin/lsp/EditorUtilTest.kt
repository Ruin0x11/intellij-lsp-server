package com.ruin.lsp

import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase
import com.ruin.lsp.util.differenceFromAction
import org.eclipse.lsp4j.TextEdit
import java.io.File

class EditorUtilTest : LightCodeInsightFixtureTestCase() {
    fun `test text edits from document differences`() {
        // workaround to avoid opening a project here
        // otherwise the next test run will complain about the project not being disposed
        // which doesn't occur when the tests are run individually
        val path = File((getProjectPath(JAVA_PROJECT) + "/" + DUMMY_FILE_PATH).replace("\\", "/"))
        val psiFile = myFixture.configureByText("Dummy.java", path.readText())

        val edits = differenceFromAction(psiFile) { editor, _ ->
            // insert
            editor.document.insertString(0, "Hey, dood!")
            // delete
            editor.document.deleteString(40, 45)
            // replace
            editor.document.deleteString(60, 65)
            editor.document.insertString(60, "blah")
        }
        assertNotNull(edits)
        assertSameElements(edits!!.toList(), listOf(
            TextEdit(range(0, 0, 0, 0), "Hey, dood!"),
            TextEdit(range(2, 0, 2, 5), ""),
            TextEdit(range(2, 25, 4, 1), "blah")
        ))
    }
// fails due to issue #39
//    fun `test text edits after document change`() {
//        val doc = getDocument(psiFile)!!
//        runUndoTransparentWriteAction {
//            doc.insertString(0, "Cowabunga!")
//        }
//        val edits = differenceFromAction(psiFile) { editor, _ ->
//            editor.document.insertString(0, "Hey, dood!")
//        }
//        assertNotNull(edits)
//        assertSameElements(edits!!.toList(), listOf(
//            TextEdit(range(0, 1, 0, 0), "Hey, d"),
//            TextEdit(range(0, 2, 0, 9), "od")
//        ))
//    }
}
