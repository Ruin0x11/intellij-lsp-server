package com.ruin.lsp.commands.document.formatting

import com.intellij.application.options.CodeStyle
import com.intellij.lang.java.JavaLanguage
import com.intellij.openapi.application.ApplicationManager
import com.intellij.psi.PsiFile
import com.intellij.psi.codeStyle.CodeStyleManager
import com.intellij.psi.codeStyle.CodeStyleSettings
import com.intellij.psi.codeStyle.CodeStyleSettingsManager
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.util.differenceFromAction
import com.ruin.lsp.util.toTextRange
import org.eclipse.lsp4j.FormattingOptions
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.TextEdit

class DocumentFormattingCommand(val options: FormattingOptions, val range: Range? = null) : DocumentCommand<MutableList<TextEdit>> {
    override fun execute(ctx: ExecutionContext): MutableList<TextEdit> {
        val styleSettings = CodeStyle.getDefaultSettings()

        val newSettings = configureSettings(styleSettings, options, ctx.file)

        CodeStyle.setTemporarySettings(ctx.project, newSettings)
        val edits = differenceFromAction(ctx.file) { editor, copy ->
            val textRange = range?.toTextRange(editor.document)
            val start = textRange?.startOffset ?: 0
            val end = textRange?.endOffset ?: copy.textLength
            ApplicationManager.getApplication().runWriteAction {
                CodeStyleManager.getInstance(copy.project).reformatText(copy, start, end)
            }
        }
        CodeStyle.dropTemporarySettings(ctx.project)

        return edits?.toMutableList() ?: mutableListOf()
    }
}

fun configureSettings(settings: CodeStyleSettings, options: FormattingOptions, file: PsiFile): CodeStyleSettings {
    settings.getCommonSettings(file.language).ALIGN_GROUP_FIELD_DECLARATIONS
    val indentOptions = settings.getIndentOptionsByFile(file)
    indentOptions.TAB_SIZE = options.tabSize
    indentOptions.USE_TAB_CHARACTER = !options.isInsertSpaces
    return settings
}
