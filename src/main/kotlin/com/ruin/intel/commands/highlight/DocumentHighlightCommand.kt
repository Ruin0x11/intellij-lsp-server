package com.ruin.intel.commands.highlight

import com.github.kittinunf.result.Result
import com.intellij.codeInsight.highlighting.HighlightUsagesHandler
import com.intellij.codeInsight.highlighting.HighlightUsagesHandlerBase
import com.intellij.codeInsight.highlighting.ReadWriteAccessDetector
import com.intellij.featureStatistics.FeatureUsageTracker
import com.intellij.find.FindManager
import com.intellij.find.findUsages.PsiElement2UsageTargetAdapter
import com.intellij.find.impl.FindManagerImpl
import com.intellij.idea.ActionsBundle
import com.intellij.lang.injection.InjectedLanguageManager
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.DumbService
import com.intellij.openapi.project.IndexNotReadyException
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Ref
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.PsiReference
import com.intellij.psi.search.LocalSearchScope
import com.intellij.psi.search.searches.ReferencesSearch
import com.intellij.usages.UsageTarget
import com.intellij.usages.UsageTargetUtil
import com.ruin.intel.Util.withEditor
import com.ruin.intel.commands.Command
import com.ruin.intel.values.*
import java.util.*

class DocumentHighlightCommand(val textDocumentIdentifier: TextDocumentIdentifier,
                               val position: Position) : Command<List<DocumentHighlight>> {
    override fun execute(project: Project, file: PsiFile): Result<List<DocumentHighlight>, Exception> {

        val ref: Ref<List<DocumentHighlight>> = Ref()
        withEditor(this, file, position) { editor ->
            try {
                ref.set(findHighlights(project, editor, file))
            } catch (ex: IndexNotReadyException) {
                DumbService.getInstance(project).showDumbModeNotification(ActionsBundle.message("action.HighlightUsagesInFile.not.ready"))
            }
        }

        return Result.of(ref.get())
    }
}


fun offsetToPosition(editor: Editor, offset: Int): Position {
    val logicalPos = editor.offsetToLogicalPosition(offset)
    return Position(logicalPos.line, logicalPos.column)
}

fun textRangeToRange(editor: Editor, range: TextRange) =
    Range(
        offsetToPosition(editor, range.startOffset),
        offsetToPosition(editor, range.endOffset)
    )


private fun findHighlights(project: Project, editor: Editor, file: PsiFile): List<DocumentHighlight>? {
    val handler = HighlightUsagesHandler.createCustomHandler(editor, file)

    if (handler != null) {
        return getHighlightsFromHandler(handler, editor)
    }

    val ref: Ref<List<DocumentHighlight>> = Ref()
    DumbService.getInstance(project).withAlternativeResolveEnabled {
        val usageTargets = getUsageTargets(editor, file)
        val result = if (usageTargets == null) {
            ref.set(listOf())
            return@withAlternativeResolveEnabled
        } else {
            usageTargets.mapNotNull { extractDocumentHighlightFromRaw(project, file, editor, it) }.flatten()
        }

        ref.set(result)
    }

    return ref.get()
}

private fun getHighlightsFromHandler(handler: HighlightUsagesHandlerBase<PsiElement>,
                                     editor: Editor): List<DocumentHighlight> {
    val featureId = handler.featureId

    if (featureId != null) {
        FeatureUsageTracker.getInstance().triggerFeatureUsed(featureId)
    }

    // FIXME: Not able to use handler.selectTargets()
    handler.computeUsages(handler.targets)

    val reads = handler.readUsages.map {
        val range = textRangeToRange(editor, it)
        DocumentHighlight(range, DocumentHighlightKind.READ)
    }

    val writes = handler.writeUsages.map {
        val range = textRangeToRange(editor, it)
        DocumentHighlight(range, DocumentHighlightKind.WRITE)
    }

    return reads.plus(writes)
}

fun findRefsToElement(target: PsiElement, project: Project, file: PsiFile) : Collection<PsiReference> {
    val findUsagesManager = (FindManager.getInstance(project) as FindManagerImpl).findUsagesManager
    val handler = findUsagesManager.getFindUsagesHandler(target, true)

    // in case of injected file, use host file to highlight all occurrences of the target in each injected file
    val context = InjectedLanguageManager.getInstance(project).getTopLevelFile(file)

    val searchScope = LocalSearchScope(context)
    return handler?.findReferencesToHighlight(target, searchScope)
        ?: ReferencesSearch.search(target, searchScope, false).findAll()
}

fun extractDocumentHighlightFromRaw(project: Project,
                                    file: PsiFile,
                                    editor: Editor,
                                    usage: UsageTarget): List<DocumentHighlight>? {
    when(usage) {
        is PsiElement2UsageTargetAdapter -> {
            val target = usage.element
            val refs = findRefsToElement(target, project, file)

            return refsToHighlights(target, file, editor, refs)
        }
    }

    return null
}

fun refsToHighlights(element: PsiElement,
                     file: PsiFile,
                     editor: Editor,
                     refs: Collection<PsiReference>): List<DocumentHighlight> {
    val detector = ReadWriteAccessDetector.findDetector(element)

    val highlights: MutableList<DocumentHighlight> = mutableListOf()

    if (detector != null) {
        val readRefs = java.util.ArrayList<PsiReference>()
        val writeRefs = java.util.ArrayList<PsiReference>()

        for (ref in refs) {
            if (detector.getReferenceAccess(element, ref) == ReadWriteAccessDetector.Access.Read) {
                readRefs.add(ref)
            } else {
                writeRefs.add(ref)
            }
        }
        addHighlights(highlights, readRefs, editor, DocumentHighlightKind.READ)
        addHighlights(highlights, writeRefs, editor, DocumentHighlightKind.WRITE)
    } else {
        addHighlights(highlights, refs, editor, DocumentHighlightKind.TEXT)
    }

    val range = HighlightUsagesHandler.getNameIdentifierRange(file, element)
    if (range != null) {
        val kind = if (detector != null && detector.isDeclarationWriteAccess(element)) {
            DocumentHighlightKind.WRITE
        } else {
            DocumentHighlightKind.TEXT
        }
        highlights.add(DocumentHighlight(textRangeToRange(editor, range), kind))
    }

    return highlights
}

private fun addHighlights(highlights: MutableList<DocumentHighlight>,
                          refs: Collection<PsiReference>,
                          editor: Editor, kind: Int) {
    val textRanges = java.util.ArrayList<TextRange>(refs.size)
    for (ref in refs) {
        HighlightUsagesHandler.collectRangesToHighlight(ref, textRanges)
    }
    val toAdd = textRanges.map { DocumentHighlight(textRangeToRange(editor, it), kind) }
    highlights.addAll(toAdd)
}

private fun getUsageTargets(editor: Editor, file: PsiFile): Array<UsageTarget>? =
    UsageTargetUtil.findUsageTargets(editor, file)

