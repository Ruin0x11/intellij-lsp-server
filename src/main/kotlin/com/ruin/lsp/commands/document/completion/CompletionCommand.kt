package com.ruin.lsp.commands.document.completion

import com.intellij.codeInsight.completion.*
import com.intellij.codeInsight.completion.impl.CamelHumpMatcher
import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.codeInsight.lookup.impl.LookupImpl
import com.intellij.openapi.util.Ref
import com.intellij.psi.PsiElement
import com.intellij.util.Consumer
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.model.CompletionResolveIndex
import com.ruin.lsp.model.PreviousCompletionCacheService
import com.ruin.lsp.util.withEditor
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.CompletionList
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.jsonrpc.CancelChecker
import org.eclipse.lsp4j.jsonrpc.messages.Either
import java.util.*

class CompletionCommand(val position: Position,
                        val snippetSupport: Boolean) : DocumentCommand<Either<MutableList<CompletionItem>, CompletionList>> {
    override fun execute(ctx: ExecutionContext): Either<MutableList<CompletionItem>, CompletionList> {
        val prefix: String? = null
        var sortedLookupElements: List<LookupElement> = listOf()

        val completionCache = PreviousCompletionCacheService.getInstance()
        val completionId = completionCache.incrementId()

        withEditor(this, ctx.file, position) { editor ->
            val lookupElements: MutableList<LookupElement> = mutableListOf()
            val params = makeCompletionParameters(editor, ctx.file, position)!!
            val arranger = MyCompletionLookupArranger(params, CompletionLocation(params))
            val lookup = LookupImpl(ctx.project, editor, arranger)

            performCompletion(params, prefix, ctx.cancelToken, Consumer { completionResult ->
                lookup.addItem(completionResult.lookupElement, CamelHumpMatcher(""))
                arranger.addElement(completionResult)

                val el = completionResult.lookupElement

                lookupElements.add(el)
                ctx.profiler?.mark("Get elt $el")
            })

            val sorted: Ref<List<LookupElement>> = Ref(listOf())
            sorted.set(arranger.arrangeItems(lookup, false).first)
            sortedLookupElements = sorted.get()
            ctx.profiler?.mark("Finish sorting")
        }

        val result = sortedLookupElements.mapIndexedNotNull { i, it ->
            val dec = CompletionDecorator.from(it, snippetSupport)
            dec?.completionItem?.apply {
                this.sortText = i.toString()
                this.data = CompletionResolveIndex(completionId, i)
            }
        }

        completionCache.cacheCompletion(ctx.file, sortedLookupElements)

        return Either.forRight(CompletionList(false, result.toMutableList()))
    }
}

fun performCompletion(parameters: CompletionParameters,
                      prefix: String?,
                      cancelToken: CancelChecker?,
                      consumer: Consumer<CompletionResult>?): Array<LookupElement> {
    val lookupSet = LinkedHashSet<LookupElement>()

    getVariantsFromContributors(parameters, prefix, null, cancelToken, Consumer { result ->
        if (lookupSet.add(result.lookupElement) && consumer != null) {
            consumer.consume(result)
        }
    })
    return lookupSet.toTypedArray()
}

/**
 * Run all contributors until any of them returns false or the list is exhausted. If from parameter is not null, contributors
 * will be run starting from the next one after that.
 */
fun getVariantsFromContributors(parameters: CompletionParameters,
                                prefix: String?, from: CompletionContributor?,
                                cancelToken: CancelChecker?,
                                consumer: Consumer<CompletionResult>) {
    val contributors = CompletionContributor.forParameters(parameters)
    for (i in contributors.indexOf(from) + 1 until contributors.size) {
        cancelToken?.checkCanceled()
        val contributor = contributors[i]

        val result = createResultSet(parameters, prefix, consumer, contributor, cancelToken)
        contributor.fillCompletionVariants(parameters, result)
        if (result.isStopped) {
            return
        }
    }
}

fun createResultSet(parameters: CompletionParameters, userPrefix: String?,
                    consumer: Consumer<CompletionResult>, contributor: CompletionContributor,
                    cancelToken: CancelChecker?): CompletionResultSet {
    val position = parameters.position
    val prefix = userPrefix ?: findPrefix(position, parameters.offset)
    val lengthOfTextBeforePosition = parameters.offset
    val matcher = CamelHumpMatcher(prefix, false)
    val sorter = CompletionService.getCompletionService().defaultSorter(parameters, matcher)
    return CompletionResultSetImpl(consumer, lengthOfTextBeforePosition, matcher,
        contributor, parameters, sorter, null, cancelToken)
}

fun findPrefix(position: PsiElement, offset: Int): String {
    // Class is deprecated, but the method seems to be used...
    return CompletionData.findPrefixStatic(position, offset)
}
