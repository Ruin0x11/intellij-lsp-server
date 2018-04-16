package com.ruin.lsp.commands.document.completion

import com.intellij.codeInsight.completion.*
import com.intellij.codeInsight.completion.impl.CompletionSorterImpl
import com.intellij.codeInsight.lookup.*
import com.intellij.codeInsight.lookup.impl.LookupImpl
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.util.Condition
import com.intellij.openapi.util.Key
import com.intellij.openapi.util.Pair
import com.intellij.openapi.util.text.StringUtil
import com.intellij.util.ProcessingContext
import com.intellij.util.containers.ContainerUtil
import com.intellij.util.containers.MultiMap
import java.util.ArrayList
import java.util.LinkedHashSet

class MyCompletionLookupArranger(val params: CompletionParameters, val location: CompletionLocation) : LookupArranger() {

    private val LOG = Logger.getInstance(MyCompletionLookupArranger::class.java)
    val myClassifiers: MutableMap<CompletionSorter, Classifier<LookupElement>> = mutableMapOf()
    val myPrefixChanges = 0
    val mySorterKey = Key.create<CompletionSorterImpl>("SORTER_KEY")
    private val myFinalSorter = CompletionFinalSorter.newSorter()
    private val PRESENTATION_INVARIANT = Key.create<PresentationInvariant>("PRESENTATION_INVARIANT")
    private val BY_PRESENTATION_COMPARATOR: java.util.Comparator<LookupElement> = Comparator { o1, o2 ->
        val invariant = PRESENTATION_INVARIANT.get(o1)!!
        invariant.compareTo(PRESENTATION_INVARIANT.get(o2))
    }
    private val MAX_PREFERRED_COUNT = 5
    val maxLookupListHeight = 10
    val items: MutableList<LookupElement> = mutableListOf()

    private val myFrozenItems = ArrayList<LookupElement>()

    fun addElement(item: CompletionResult) {
        val presentation = LookupElementPresentation()
        item.lookupElement.renderElement(presentation)

        StatisticsWeigher.clearBaseStatisticsInfo(item.lookupElement)

        val invariant = PresentationInvariant(presentation.itemText, presentation.tailText, presentation.typeText)
        item.lookupElement.putUserData(PRESENTATION_INVARIANT, invariant)

        items.add(item.lookupElement)
        associateSorter(item.lookupElement, item.sorter as CompletionSorterImpl)

        super.addElement(item.lookupElement, presentation)
    }

    private fun associateSorter(element: LookupElement, sorter: CompletionSorterImpl) {
        element.putUserData(mySorterKey, sorter)
    }

    override fun arrangeItems(lookup: Lookup, onExplicitAction: Boolean): Pair<MutableList<LookupElement>, Int> {
        val sortedByRelevance = sortByRelevance(groupItemsBySorter(items))

        val relevantSelection = findMostRelevantItem(sortedByRelevance)
        val lookupImpl = lookup as LookupImpl
        val isAlphaSorted = false
        val listModel = if (isAlphaSorted)
            sortByPresentation(items)
        else
            fillModelByRelevance(lookupImpl, ContainerUtil.newIdentityTroveSet<LookupElement>(items), sortedByRelevance, relevantSelection)

        //val toSelect = getItemToSelect(lookupImpl, listModel, onExplicitAction, relevantSelection)
        //LOG.assertTrue(toSelect >= 0)
        val toSelect = 0

        //addDummyItems(items.size - listModel.size, listModel)

        return Pair(listModel.toMutableList(), toSelect)
    }

    override fun createEmptyCopy(): LookupArranger {
        return MyCompletionLookupArranger(params, location)
    }


    private fun groupItemsBySorter(source: Iterable<LookupElement>): MultiMap<CompletionSorterImpl, LookupElement> {
        val inputBySorter = MultiMap.createLinked<CompletionSorterImpl, LookupElement>()
        for (element in source) {
            inputBySorter.putValue(obtainSorter(element), element)
        }
        for (sorter in inputBySorter.keySet()) {
            inputBySorter.put(sorter, sortByPresentation(inputBySorter.get(sorter)))
        }

        return inputBySorter
    }

    private fun sortByPresentation(source: Iterable<LookupElement>): List<LookupElement> {
        val startMatches = ContainerUtil.newArrayList<LookupElement>()
        val middleMatches = ContainerUtil.newArrayList<LookupElement>()
        for (element in source) {
            (if (itemMatcher(element).isStartMatch(element)) startMatches else middleMatches).add(element)
        }
        ContainerUtil.sort(startMatches, BY_PRESENTATION_COMPARATOR)
        ContainerUtil.sort(middleMatches, BY_PRESENTATION_COMPARATOR)
        startMatches.addAll(middleMatches)
        return startMatches
    }

    private fun fillModelByRelevance(lookup: LookupImpl,
                                     items: Set<LookupElement>,
                                     sortedElements: Iterable<LookupElement>,
                                     relevantSelection: LookupElement?): List<LookupElement> {
        val byRelevance = sortedElements.iterator()

        val model = LinkedHashSet<LookupElement>()

        addPrefixItems(model)
        addFrozenItems(items, model)
        if (model.size < MAX_PREFERRED_COUNT) {
            addSomeItems(model, byRelevance, Condition { lastAdded -> model.size >= MAX_PREFERRED_COUNT })
        }

        freezeTopItems(lookup, model)

        ensureItemAdded(items, model, byRelevance, lookup.currentItem)
        ensureItemAdded(items, model, byRelevance, relevantSelection)
        ensureEverythingVisibleAdded(lookup, model, byRelevance)

        return ArrayList(model)
    }

    private fun sortByRelevance(inputBySorter: MultiMap<CompletionSorterImpl, LookupElement>): Iterable<LookupElement> {
        val byClassifier = ContainerUtil.newArrayList<Iterable<LookupElement>>()
        for (sorter in myClassifiers.keys) {
            val context = createContext()
            byClassifier.add(myClassifiers.get(sorter)!!.classify(inputBySorter.get(sorter as CompletionSorterImpl), context))
        }

        val result: Iterable<LookupElement> = ContainerUtil.concat<LookupElement>(*byClassifier.toTypedArray<Iterable<LookupElement>>())
        return myFinalSorter.sort(result, params)
    }

    private fun findMostRelevantItem(sorted: Iterable<LookupElement>): LookupElement? {
        val skippers = CompletionPreselectSkipper.EP_NAME.extensions

        for (element in sorted) {
            if (!shouldSkip(skippers, element)) {
                return element
            }
        }

        return null
    }

    private fun shouldSkip(skippers: Array<CompletionPreselectSkipper>, element: LookupElement): Boolean {
        for (skipper in skippers) {
            if (skipper.skipElement(element, location)) {
                if (LOG.isDebugEnabled) {
                    LOG.debug("Skipped element $element by $skipper")
                }
                return true
            }
        }
        return false
    }

    private fun createContext(): ProcessingContext {
        val context = ProcessingContext()
        context.put<Int>(CompletionLookupArranger.PREFIX_CHANGES, myPrefixChanges)
        context.put<WeighingContext>(CompletionLookupArranger.WEIGHING_CONTEXT, this)
        return context
    }


    private fun obtainSorter(element: LookupElement): CompletionSorterImpl {
        return element.getUserData(mySorterKey)!!
    }


    private fun ensureEverythingVisibleAdded(lookup: LookupImpl, model: LinkedHashSet<LookupElement>, byRelevance: Iterator<LookupElement>) {
        val list = lookup.list
        val testMode = ApplicationManager.getApplication().isUnitTestMode
        val limit = Math.max(list.lastVisibleIndex, model.size) + maxLookupListHeight * 3
        addSomeItems(model, byRelevance, Condition { lastAdded -> !testMode && model.size >= limit })
    }

    private fun ensureItemAdded(items: Set<LookupElement>,
                                model: LinkedHashSet<LookupElement>,
                                byRelevance: Iterator<LookupElement>, item: LookupElement?) {
        if (item != null && items.contains(item) && !model.contains(item)) {
            addSomeItems(model, byRelevance, Condition { lastAdded -> lastAdded === item })
        }
    }

    private fun freezeTopItems(lookup: LookupImpl, model: LinkedHashSet<LookupElement>) {
        myFrozenItems.clear()
        if (lookup.isShown) {
            myFrozenItems.addAll(model)
        }
    }

    private fun addFrozenItems(items: Set<LookupElement>, model: LinkedHashSet<LookupElement>) {
        val iterator = myFrozenItems.iterator()
        while (iterator.hasNext()) {
            val element = iterator.next()
            if (!element.isValid() || !items.contains(element)) {
                iterator.remove()
            }
        }
        model.addAll(myFrozenItems)
    }

    private fun addPrefixItems(model: LinkedHashSet<LookupElement>) {
        ContainerUtil.addAll(model, sortByRelevance(groupItemsBySorter(getPrefixItems(true))))
        ContainerUtil.addAll(model, sortByRelevance(groupItemsBySorter(getPrefixItems(false))))
    }

    private fun addSomeItems(model: LinkedHashSet<LookupElement>, iterator: Iterator<LookupElement>, stopWhen: Condition<LookupElement>) {
        while (iterator.hasNext()) {
            val item = iterator.next()
            model.add(item)
            if (stopWhen.value(item)) {
                break
            }
        }
    }
}

internal data class PresentationInvariant(val itemText: String?, val tail: String?, val type: String?): Comparable<PresentationInvariant> {
    override fun compareTo(other: PresentationInvariant): Int {
        var result = StringUtil.naturalCompare(itemText, other.itemText)
        if (result != 0) return result

        result = Integer.compare(tail?.length ?: 0, other.tail?.length ?: 0)
        if (result != 0) return result

        result = StringUtil.naturalCompare(tail ?: "", other.tail ?: "")
        if (result != 0) return result

        return StringUtil.naturalCompare(type ?: "", other.type ?: "")
    }

}
