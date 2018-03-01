package com.ruin.intel.commands.find

import com.github.kittinunf.result.Result
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Factory
import com.intellij.psi.PsiFile
import com.intellij.usages.*
import com.ruin.intel.commands.Command
import com.ruin.intel.values.Location
import com.ruin.intel.values.Position
import com.ruin.intel.values.TextDocumentIdentifier
import com.intellij.usages.UsageViewManager.UsageViewStateListener
import com.intellij.util.Processor
import com.intellij.usages.UsageInfo2UsageAdapter
import com.intellij.psi.PsiElement
import com.intellij.util.containers.ContainerUtil
import com.intellij.openapi.editor.Editor
import com.ruin.intel.Util.createEditor
import com.intellij.find.findUsages.FindUsagesManager
import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.util.Ref
import com.ruin.intel.Util.findTargetElement
import com.ruin.intel.Util.getDocument
import com.ruin.intel.Util.withEditor
import com.ruin.intel.commands.errorResult
import com.ruin.intel.model.positionToOffset
import java.util.ArrayList

class FindUsagesCommand(val textDocumentIdentifier: TextDocumentIdentifier,
                        val position: Position) : Command<List<Location>> {
    override fun execute(project: Project, file: PsiFile): Result<List<Location>, Exception> {
        val doc = getDocument(file) ?: return errorResult("No document found")
        val offset = positionToOffset(doc, position)

        val element = file.findElementAt(offset) ?: return errorResult("No element at position")

        val ref: Ref<List<Usage>> = Ref()
        withEditor(this, file, position) { editor ->
            ref.set(findUsages(editor))
        }
        val rawResults = ref.get()

        if (rawResults.isEmpty()) {
            return Result.of(listOf())
        }

        val results = rawResults.mapNotNull(::extractLocationFromRaw)

        return Result.of(results)
    }
}

fun extractLocationFromRaw(usage: Usage): Location? {
    if (usage is UsageInfo2UsageAdapter) {
        val element = usage.element
        if (element != null) {
            return toLocation(element)
        }
    }
    return null
}

fun findUsages(editor: Editor): List<Usage> {
    val project = editor.project ?: return listOf()

    val element = findTargetElement(editor)

    val rawResults = ArrayList<Usage>()
    val manager = FindUsagesManager(project,
        UsageCollectingViewManager(project, rawResults))
    manager.findUsages(element, null, null, false, null)
    return rawResults
}

internal class UsageCollectingViewManager(val project: Project, private val results: MutableList<Usage>) : UsageViewManager(), Processor<Usage> {
    override fun showUsages(searchedFor: Array<out UsageTarget>,
                            foundUsages: Array<out Usage>,
                            presentation: UsageViewPresentation,
                            factory: Factory<UsageSearcher>?): UsageView {
        return showUsages(searchedFor, foundUsages, presentation)
    }

    override fun showUsages(searchedFor: Array<out UsageTarget>,
                            foundUsages: Array<out Usage>,
                            presentation: UsageViewPresentation): UsageView {
        for (usage in foundUsages) {
            process(usage)
        }

        return UsageViewManager.getInstance(project)
            .createUsageView(UsageTarget.EMPTY_ARRAY, Usage.EMPTY_ARRAY, presentation, null)
    }

    override fun getSelectedUsageView() = null

    override fun searchAndShowUsages(searchFor: Array<out UsageTarget>,
                                     searcherFactory: Factory<UsageSearcher>,
                                     showPanelIfOnlyOneUsage: Boolean,
                                     showNotFoundMessage: Boolean,
                                     presentation: UsageViewPresentation,
                                     listener: UsageViewStateListener?): UsageView? {
        searcherFactory.create().generate(this)
        return null
    }

    override fun searchAndShowUsages(searchFor: Array<out UsageTarget>,
                                     searcherFactory: Factory<UsageSearcher>,
                                     processPresentation: FindUsagesProcessPresentation,
                                     presentation: UsageViewPresentation,
                                     listener: UsageViewStateListener?) {
        searcherFactory.create().generate(this)
    }

    override fun createUsageView(targets: Array<out UsageTarget>,
                                 usages: Array<out Usage>,
                                 presentation: UsageViewPresentation,
                                 usageSearcherFactory: Factory<UsageSearcher>?): UsageView {
        return showUsages(targets, usages, presentation)
    }

    override fun process(usage: Usage?): Boolean {
        if(usage != null)
            results.add(usage)
        return true
    }
}
