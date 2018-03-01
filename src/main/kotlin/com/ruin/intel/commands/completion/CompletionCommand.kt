package com.ruin.intel.commands.completion

import com.github.kittinunf.result.Result
import com.intellij.codeInsight.completion.*
import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.openapi.Disposable
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.EditorFactory
import com.ruin.intel.Util.createEditor
import com.ruin.intel.Util.resolvePsiFromUri
import com.ruin.intel.values.CompletionItem
import com.ruin.intel.values.CompletionList
import com.ruin.intel.values.Position
import com.ruin.intel.values.TextDocumentIdentifier
import com.intellij.psi.PsiElement
import com.intellij.codeInsight.completion.impl.CamelHumpMatcher
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile
import com.intellij.util.Consumer
import com.ruin.intel.commands.Command
import java.util.LinkedHashSet

class CompletionCommand(val textDocumentIdentifier: TextDocumentIdentifier,
                        val position: Position,
                        val triggerKind: Int?,
                        val triggerCharacter: String?) : Command<CompletionList>, Disposable {

    override fun execute(project: Project, file: PsiFile): Result<CompletionList, Exception> {
        val result: MutableList<CompletionItem> = mutableListOf()
        val prefix: String? = null

        val editor = createEditor(this, file, position.line, position.character)
        val params = makeCompletionParameters(editor, file, position)

        performCompletion(params!!, prefix, Consumer { completionResult ->
            val el = completionResult.lookupElement
            val dec = CompletionDecorator.from(el)
            if (dec != null) {
                result.add(dec.completionItem)
            }
        })

        // Disposer doesn't release editor after registering in createEditor?
        val editorFactory = EditorFactory.getInstance()
        editorFactory.releaseEditor(editor)

        return Result.of(CompletionList(false, result))
    }
}

fun performCompletion(parameters: CompletionParameters,
                      prefix: String?,
                      consumer: Consumer<CompletionResult>?): Array<LookupElement> {


    val lookupSet = LinkedHashSet<LookupElement>()

    getVariantsFromContributors(parameters, prefix, null, Consumer { result ->
        if (lookupSet.add(result.lookupElement) && consumer != null) {
            consumer.consume(result)
        }
    })
    return lookupSet.toTypedArray()
}

/**
 * Run all contributors until any of them returns false or the list is exhausted. If makeCompletionParameters parameter is not null, contributors
 * will be run starting makeCompletionParameters the next one after that.
 */
fun getVariantsFromContributors(parameters: CompletionParameters,
                                prefix: String?, from: CompletionContributor?,
                                consumer: Consumer<CompletionResult>) {
    val contributors = CompletionContributor.forParameters(parameters)
    for (i in contributors.indexOf(from) + 1 until contributors.size) {
        val contributor = contributors[i]

        val result = createResultSet(parameters, prefix, consumer, contributor)
        contributor.fillCompletionVariants(parameters, result)
        if (result.isStopped) {
            return
        }
    }

}

fun createResultSet(parameters: CompletionParameters, userPrefix: String?,
                    consumer: Consumer<CompletionResult>, contributor: CompletionContributor): CompletionResultSet {
    val position = parameters.position
    val prefix = userPrefix ?: findPrefix(position, parameters.offset)
    val lengthOfTextBeforePosition = parameters.offset
    val matcher = CamelHumpMatcher(prefix, false)
    val sorter = CompletionService.getCompletionService().defaultSorter(parameters, matcher)
    return CompletionResultSetImpl(consumer, lengthOfTextBeforePosition, matcher,
        contributor, parameters, sorter, null)
}

fun findPrefix(position: PsiElement, offset: Int): String {
    // Class is deprecated, but the method seems to be used...
    return CompletionData.findPrefixStatic(position, offset)
}
