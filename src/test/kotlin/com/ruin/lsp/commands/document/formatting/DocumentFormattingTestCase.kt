package com.ruin.lsp.commands.document.formatting

import com.intellij.ide.highlighter.JavaFileType
import com.intellij.openapi.fileTypes.FileType
import com.ruin.lsp.range
import org.eclipse.lsp4j.FormattingOptions

class DocumentFormattingTestCase : DocumentFormattingTestBase() {
    override val fileType = JavaFileType.INSTANCE

    val TESTCASE = """
        public class Dood        {
        public static  void main(String[  ]    args){
        System.out.println( " Hey, dood!")  ;
        }
        }
"""

    fun `test formats whole document`() = checkDocumentFormat(TESTCASE,
        """public class Dood {
    public static void main(String[] args) {
        System.out.println(" Hey, dood!");
    }
}
""", FormattingOptions(2, true))
    fun `test formats whole document with tabs`() = checkDocumentFormat(TESTCASE,
        """public class Dood {
		public static void main(String[] args) {
				System.out.println(" Hey, dood!");
		}
}
""", FormattingOptions(2, false))

    fun `test formats range`() = checkDocumentFormat(TESTCASE,
        """
        public class Dood        {
            public static void main(String[] args) {
                System.out.println(" Hey, dood!");
            }
        }
""", FormattingOptions(2, true), range(2, 0, 5, 0))
}
