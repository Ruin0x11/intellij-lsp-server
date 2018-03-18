package com.ruin.lsp.commands.document.highlight

import com.intellij.codeInsight.TargetElementUtil
import com.intellij.codeInsight.highlighting.HighlightUsagesHandler
import com.intellij.codeInsight.highlighting.HighlightUsagesHandlerBase
import com.intellij.codeInsight.highlighting.ReadWriteAccessDetector
import com.intellij.featureStatistics.FeatureUsageTracker
import com.intellij.find.FindManager
import com.intellij.find.findUsages.PsiElement2UsageTargetAdapter
import com.intellij.find.impl.FindManagerImpl
import com.intellij.lang.injection.InjectedLanguageManager
import com.intellij.navigation.NavigationItem
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.DumbService
import com.intellij.openapi.project.IndexNotReadyException
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Ref
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.PsiPolyVariantReference
import com.intellij.psi.PsiReference
import com.intellij.psi.search.LocalSearchScope
import com.intellij.psi.search.searches.ReferencesSearch
import com.intellij.usages.UsageTarget
import com.intellij.usages.UsageTargetUtil
import com.intellij.util.containers.ContainerUtil
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.util.findTargetElement
import com.ruin.lsp.util.withEditor
import org.eclipse.lsp4j.DocumentHighlight
import org.eclipse.lsp4j.DocumentHighlightKind
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.Range

class DocumentHighlightCommand(val position: Position) : DocumentCommand<MutableList<DocumentHighlight>> {
    override fun execute(ctx: ExecutionContext): MutableList<DocumentHighlight> {
        val ref: Ref<List<DocumentHighlight>> = Ref()
        withEditor(this, ctx.file, position) { editor ->
            try {
                ref.set(findHighlights(ctx.project, editor, ctx.file))
            } catch (ex: IndexNotReadyException) {
                return@withEditor
            }
        }

        return ref.get().toMutableList()
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

    return if (handler != null) {
        getHighlightsFromHandler(handler, editor)
    } else {
        getHighlightsFromUsages(project, editor, file)
    }
}

private fun getHighlightsFromHandler(handler: HighlightUsagesHandlerBase<PsiElement>,
                                     editor: Editor): List<DocumentHighlight> {
    val featureId = handler.featureId

    if (featureId != null) {
        FeatureUsageTracker.getInstance().triggerFeatureUsed(featureId)
    }

    // NOTE: Not able to use handler.selectTargets()
    handler.computeUsages(handler.targets)

    val reads = textRangesToHighlights(handler.readUsages, editor, DocumentHighlightKind.Read)
    val writes = textRangesToHighlights(handler.writeUsages, editor, DocumentHighlightKind.Write)

    return reads.plus(writes)
}

private fun textRangesToHighlights(usages: List<TextRange>, editor: Editor, kind: DocumentHighlightKind): List<DocumentHighlight> =
    usages.map {
        val range = textRangeToRange(editor, it)
        DocumentHighlight(range, kind)
    }

private fun getHighlightsFromUsages(project: Project, editor: Editor, file: PsiFile): List<DocumentHighlight>? {
    val ref: Ref<List<DocumentHighlight>> = Ref()
    DumbService.getInstance(project).withAlternativeResolveEnabled {
        val usageTargets = getUsageTargets(editor, file)
        val result = usageTargets?.mapNotNull {
            extractDocumentHighlightFromRaw(project, file, editor, it)
        }?.flatten() ?: listOf()

        ref.set(result)
    }

    return ref.get()
}

private fun findRefsToElement(target: PsiElement, project: Project, file: PsiFile): Collection<PsiReference> {
    val findUsagesManager = (FindManager.getInstance(project) as FindManagerImpl).findUsagesManager
    val handler = findUsagesManager.getFindUsagesHandler(target, true)

    // in case of injected file, use host file to highlight all occurrences of the target in each injected file
    val context = InjectedLanguageManager.getInstance(project).getTopLevelFile(file)

    val searchScope = LocalSearchScope(context)
    return handler?.findReferencesToHighlight(target, searchScope)
        ?: ReferencesSearch.search(target, searchScope, false).findAll()
}

private fun extractDocumentHighlightFromRaw(project: Project,
                                            file: PsiFile,
                                            editor: Editor,
                                            usage: UsageTarget): List<DocumentHighlight>? {
    return if (usage is PsiElement2UsageTargetAdapter) {
        val target = usage.element
        val refs = findRefsToElement(target, project, file)

        return refsToHighlights(target, file, editor, refs)
    } else {
        null
    }
}

private fun refsToHighlights(element: PsiElement,
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
        addHighlights(highlights, readRefs, editor, DocumentHighlightKind.Read)
        addHighlights(highlights, writeRefs, editor, DocumentHighlightKind.Write)
    } else {
        addHighlights(highlights, refs, editor, DocumentHighlightKind.Text)
    }

    val range = HighlightUsagesHandler.getNameIdentifierRange(file, element)
    if (range != null) {
        val kind = if (detector != null && detector.isDeclarationWriteAccess(element)) {
            DocumentHighlightKind.Write
        } else {
            DocumentHighlightKind.Text
        }
        highlights.add(DocumentHighlight(textRangeToRange(editor, range), kind))
    }

    return highlights
}

private fun addHighlights(highlights: MutableList<DocumentHighlight>,
                          refs: Collection<PsiReference>,
                          editor: Editor, kind: DocumentHighlightKind) {
    val textRanges = java.util.ArrayList<TextRange>(refs.size)
    for (ref in refs) {
        HighlightUsagesHandler.collectRangesToHighlight(ref, textRanges)
    }
    val toAdd = textRanges.map { DocumentHighlight(textRangeToRange(editor, it), kind) }
    highlights.addAll(toAdd)
}

private fun getUsageTargets(editor: Editor, file: PsiFile): Array<UsageTarget>? {
    var usageTargets = UsageTargetUtil.findUsageTargets(editor, file)

    if (usageTargets == null) {
        usageTargets = getUsageTargetsFromNavItem(editor, file)
    }

    if (usageTargets == null) {
        usageTargets = getUsageTargetsFromPolyVariantReference(editor)
    }

    return usageTargets
}

private fun getUsageTargetsFromNavItem(editor: Editor, file: PsiFile): Array<UsageTarget>? {
    var targetElement = findTargetElement(editor) ?: return null
    if (targetElement !== file) {
        if (targetElement !is NavigationItem) {
            targetElement = targetElement.navigationElement
        }
        if (targetElement is NavigationItem) {
            return arrayOf(PsiElement2UsageTargetAdapter(targetElement))
        }
    }
    return null
}

private fun getUsageTargetsFromPolyVariantReference(editor: Editor): Array<UsageTarget>? {
    val ref = TargetElementUtil.findReference(editor)

    if (ref is PsiPolyVariantReference) {
        val results = ref.multiResolve(false)

        if (results.isNotEmpty()) {
            return ContainerUtil.mapNotNull(results, { result ->
                val element = result.element
                if (element == null) null else PsiElement2UsageTargetAdapter(element)
            }, UsageTarget.EMPTY_ARRAY)
        }
    }
    return null
}
