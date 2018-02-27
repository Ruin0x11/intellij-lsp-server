package com.ruin.intel.commands

import com.github.kittinunf.result.Result
import com.googlecode.jsonrpc4j.ErrorResolver
import com.intellij.codeInsight.completion.CodeCompletionHandlerBase
import com.intellij.codeInsight.completion.CompletionProgressIndicator
import com.intellij.codeInsight.completion.CompletionType
import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.codeInsight.lookup.LookupManager
import com.intellij.openapi.Disposable
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.EditorFactory
import com.intellij.psi.PsiFile
import com.intellij.psi.impl.source.tree.injected.InjectedLanguageUtil
import com.intellij.util.ui.UIUtil
import com.ruin.intel.Util.createEditor
import com.ruin.intel.model.LanguageServerException
import com.ruin.intel.model.workspace
import com.ruin.intel.values.CompletionItem
import com.ruin.intel.values.Position
import com.ruin.intel.values.TextDocumentIdentifier
import org.junit.Assert.assertNotNull

class CompletionCommand(val textDocumentIdentifier: TextDocumentIdentifier,
                        val position: Position,
                        val triggerKind: Int?,
                        val triggerCharacter: String?) : Command<List<CompletionItem>>, Disposable {

    override fun dispose() {

    }

    override fun execute(): Result<List<CompletionItem>, Exception> {
        val file = workspace().getExistingPsiFile(textDocumentIdentifier.uri)

        val editor = createEditor(this, file, position.line, position.character)
        val noCompletionsFound = invokeCompletion(editor, file)

        val elements = getLookupElements(editor, noCompletionsFound)
            ?: return Result.error(LanguageServerException(ErrorResolver.JsonError.BULK_ERROR))

        val items = elements.map { CompletionItem(it.lookupString) }

        // Disposer doesn't release editor after registering in createEditor?
        val editorFactory = EditorFactory.getInstance()
        editorFactory.releaseEditor(editor)

        return Result.of(items)
    }

    private fun getCompletionEditor(editor: Editor, file: PsiFile): Editor? {
        return InjectedLanguageUtil.getEditorForInjectedLanguageNoCommit(editor, file)
    }

    private fun invokeCompletion(editor: Editor, file: PsiFile): Boolean {
        var noCompletionsFound = false

        UIUtil.invokeAndWaitIfNeeded(Runnable {
            val handler = object : CodeCompletionHandlerBase(CompletionType.BASIC) {
                override fun completionFinished(indicator: CompletionProgressIndicator, hasModifiers: Boolean) {
                    noCompletionsFound = indicator.lookup.items.isEmpty()
                    super.completionFinished(indicator, hasModifiers)
                }
            }
            val completionEditor = getCompletionEditor(editor, file)
            assertNotNull(editor)
            handler.invokeCompletion(file.project, completionEditor!!, 1)
        })

        return noCompletionsFound
    }

    private fun getLookupElements(editor: Editor, noCompletionsFound: Boolean): Array<LookupElement>? {
        val lookup = LookupManager.getActiveLookup(editor)

        return if (lookup == null) {
            if (noCompletionsFound) LookupElement.EMPTY_ARRAY else null
        } else {
            val list = lookup.items
            list.toTypedArray()
        }
    }
}
