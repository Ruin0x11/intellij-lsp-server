package com.ruin.lsp.commands.document.find

import com.intellij.find.findUsages.FindUsagesManager
import com.intellij.openapi.application.TransactionGuard
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Factory
import com.intellij.openapi.util.Ref
import com.intellij.usages.*
import com.intellij.util.Processor
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.commands.project.symbol.toSymbolInformation
import com.ruin.lsp.util.*
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.jsonrpc.CancelChecker
import java.util.*

class FindUsagesCommand(val position: Position) : DocumentCommand<MutableList<Location>> {
    override fun execute(ctx: ExecutionContext): MutableList<Location> {
        val ref: Ref<List<Usage>> = Ref(listOf())
        withEditor(this, ctx.file, position) { editor ->
            ref.set(findUsages(editor, ctx.cancelToken))
        }
        val rawResults = ref.get()

        if (rawResults.isEmpty()) {
            return mutableListOf()
        }

        return rawResults.mapNotNull(::extractLocationFromRaw).toMutableList()
    }
}

fun extractLocationFromRaw(usage: Usage): Location? {
    return if (usage is UsageInfo2UsageAdapter) {
         usage.element?.sourceLocationIfPossible()
    } else {
        null
    }
}

fun findUsages(editor: Editor, cancelToken: CancelChecker?): List<Usage> {
    val project = editor.project ?: return listOf()

    val element = findTargetElement(editor) ?: return listOf()

    val rawResults = ArrayList<Usage>()
    val manager = FindUsagesManager(project,
        UsageCollectingViewManager(project, rawResults, cancelToken))
    TransactionGuard.getInstance().submitTransactionAndWait {
        manager.findUsages(element, null, null, false, null)
    }
    return rawResults
}

internal class UsageCollectingViewManager(val project: Project,
                                          private val results: MutableList<Usage>,
                                          private val cancelToken: CancelChecker?)
    : UsageViewManager(), Processor<Usage> {
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
        cancelToken?.checkCanceled()
        if (usage != null)
            results.add(usage)
        return true
    }
}
