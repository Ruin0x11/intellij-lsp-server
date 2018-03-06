package com.ruin.lsp.commands.completion


import com.intellij.codeInsight.completion.*
import com.intellij.codeInsight.completion.impl.CamelHumpMatcher
import com.intellij.codeInsight.completion.impl.CompletionSorterImpl
import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.patterns.ElementPattern
import com.intellij.util.Consumer
import org.eclipse.lsp4j.jsonrpc.CancelChecker

/**
 * Created by dhleong on 11/5/14.
 */
internal class CompletionResultSetImpl(consumer: Consumer<CompletionResult>, private val myLengthOfTextBeforePosition: Int,
                                       prefixMatcher: PrefixMatcher,
                                       private val contributor: CompletionContributor,
                                       private val parameters: CompletionParameters,
                                       private val sorter: CompletionSorter,
                                       private val original: CompletionResultSetImpl?,
                                       private val cancelToken: CancelChecker?) : CompletionResultSet(prefixMatcher, consumer, contributor) {

    override fun addElement(element: LookupElement) {
        if (!element.isValid) {
            println("Invalid lookup element: " + element)
            return
        }

        val matched = CompletionResult.wrap(element, prefixMatcher, sorter)
        if (matched != null) {
            passResult(matched)
        }
    }

    override fun withPrefixMatcher(matcher: PrefixMatcher): CompletionResultSet {
        return CompletionResultSetImpl(consumer, myLengthOfTextBeforePosition, matcher, contributor, parameters, sorter, this, cancelToken)
    }

    override fun stopHere() {
        //        if (LOG.isDebugEnabled()) {
        //            LOG.debug("Completion stopped\n" + DebugUtil.currentStackTrace());
        //        }
        super.stopHere()
        original?.stopHere()
    }

    override fun withPrefixMatcher(prefix: String): CompletionResultSet {
        return if (!prefix.isEmpty()) {
            // don't erase our prefix!
            // also, use `cloneWithPrefix` so our settings are preserved
            withPrefixMatcher(prefixMatcher.cloneWithPrefix(prefix))
        } else this

    }

    override fun withRelevanceSorter(sorter: CompletionSorter): CompletionResultSet {
        return CompletionResultSetImpl(consumer, myLengthOfTextBeforePosition, prefixMatcher,
            contributor, parameters, sorter as CompletionSorterImpl, this, cancelToken)
    }

    override fun addLookupAdvertisement(text: String) {
        completionService.advertisementText = text
    }

    override fun caseInsensitive(): CompletionResultSet {
        return withPrefixMatcher(CamelHumpMatcher(prefixMatcher.prefix, false))
    }

    override fun restartCompletionOnPrefixChange(prefixCondition: ElementPattern<String>) {
        //        System.out.println("restartCompletionOnPrefixChange:" + prefixCondition);
        //        final CompletionProgressIndicator indicator = getCompletionService().getCurrentCompletion();
        //        if (indicator != null) {
        //            indicator.addWatchedPrefix(myLengthOfTextBeforePosition - getPrefixMatcher().getPrefix().length(), prefixCondition);
        //        }
    }

    override fun restartCompletionWhenNothingMatches() {
        //        System.out.println("restartCompletionWhenNothingMatches");
        //        final CompletionProgressIndicator indicator = getCompletionService().getCurrentCompletion();
        //        if (indicator != null) {
        //            indicator.getLookup().setStartCompletionWhenNothingMatches(true);
        //        }
    }

    override fun runRemainingContributors(parameters: CompletionParameters, consumer: Consumer<CompletionResult>, stop: Boolean) {
        if (stop) {
            stopHere()
        }
        getVariantsFromContributors(parameters, prefixMatcher.prefix, contributor, cancelToken, consumer)
    }

    companion object {
        val completionService: CompletionService
            get() = CompletionService.getCompletionService()
    }
}
