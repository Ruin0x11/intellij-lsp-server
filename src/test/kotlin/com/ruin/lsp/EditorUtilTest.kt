package com.ruin.lsp

import com.ruin.lsp.util.differenceFromAction
import org.eclipse.lsp4j.TextEdit
import org.intellivim.FileEditingTestCase

class EditorUtilTest : FileEditingTestCase() {
    override val projectName = JAVA_PROJECT
    override val filePath = DUMMY_FILE_PATH

    fun `test text edits from document differences`() {
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
