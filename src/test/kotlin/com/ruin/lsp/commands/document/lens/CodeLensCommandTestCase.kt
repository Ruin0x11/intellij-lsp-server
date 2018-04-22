package com.ruin.lsp.commands.document.lens

import com.ruin.lsp.model.RunConfigurationData
import com.ruin.lsp.model.RunConfigurationDescription
import com.ruin.lsp.model.RunConfigurationState
import com.ruin.lsp.range
import org.eclipse.lsp4j.CodeLens
import org.eclipse.lsp4j.Command

// Can't seem to test this, gutters are never found even after copying the tests in RunLineMarkerTest verbatim

//class CodeLensCommandTestCase : CodeLensCommandTestBase() {
//    fun `test finds main function`() =
//        doTest("MainTest.java", """
//            public class MainTest {
//               public static void <caret>foo(String[] args) {
//               }
//               public static void main(String[] args) {
//               }
//            }
//            """.trimIndent(), listOf(
//            CodeLens(range(0,0,0,0), Command(), RunConfigurationData(
//                RunConfigurationDescription("","",""),
//                RunConfigurationState.Run
//            ))
//        ))
//}
