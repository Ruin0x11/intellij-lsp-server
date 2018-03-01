package com.ruin.intel.commands.definition

import com.github.kittinunf.result.Result
import com.intellij.codeInsight.TargetElementUtil
import com.intellij.codeInsight.navigation.actions.GotoDeclarationAction
import com.intellij.debugger.SourcePosition
import com.intellij.extapi.psi.PsiElementBase
import com.intellij.ide.util.EditSourceUtil
import com.intellij.ide.util.PsiNavigationSupport
import com.intellij.lang.LanguageNamesValidation
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.fileEditor.OpenFileDescriptor
import com.intellij.openapi.project.DumbService
import com.intellij.openapi.project.IndexNotReadyException
import com.intellij.openapi.project.Project
import com.intellij.pom.Navigatable
import com.intellij.psi.PsiClass
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.PsiReference
import com.intellij.psi.util.PsiUtilCore
import com.ruin.intel.Util.createEditor
import com.ruin.intel.Util.getDocument
import com.ruin.intel.Util.getURIForFile
import com.ruin.intel.Util.resolvePsiFromUri
import com.ruin.intel.commands.completion.Command
import com.ruin.intel.model.positionToOffset
import com.ruin.intel.values.Location
import com.ruin.intel.values.Position
import com.ruin.intel.values.Range
import com.ruin.intel.values.TextDocumentIdentifier
import java.lang.Integer.max
import java.util.*

class DefinitionCommand(val textDocumentIdentifier: TextDocumentIdentifier,
                        val position: Position) : Command<List<Location>> {
    override fun execute(): Result<List<Location>, Exception> {
        var result: List<Location> = listOf()

        ApplicationManager.getApplication().invokeAndWait {
            val pair = resolvePsiFromUri(textDocumentIdentifier.uri)
            if (pair != null) {
                val (project, file) = pair

                DumbService.getInstance(project).isAlternativeResolveEnabled = true
                val editor = createEditor(this, file, position.line, position.character)
                val originalElement = file.findElementAt(editor.caretModel.offset)
                val doc = getDocument(file)

                try {
                    val navElements = findTargetElements(project, editor, file, position)

                    if (navElements != null) {
                        result = navElements
                            .mapNotNull(::getNavagatable)
                            .mapNotNull(::findLocation)
                    }
                } catch (e: IndexNotReadyException) {
                    DumbService.getInstance(project).showDumbModeNotification("Navigation is not available here during index update");
                } finally {
                    val editorFactory = EditorFactory.getInstance()
                    editorFactory.releaseEditor(editor)
                    DumbService.getInstance(project).isAlternativeResolveEnabled = false;
                }
            }
        }
        return Result.of(result)
    }
}

fun findLocation(nav: Navigatable): Location? {
    // TODO: support lookup of files inside JARs?
    return when(nav) {
        is PsiElement -> {
            val canNavigate = PsiNavigationSupport.getInstance().canNavigate(nav)
            if (canNavigate) {
                val psiNav = PsiNavigationSupport.getInstance().getDescriptor(nav)
                when(psiNav) {
                    is OpenFileDescriptor -> {
                        val uri = getURIForFile(psiNav.file)
                        val doc = getDocument(nav.containingFile)

                        val position = if (doc != null) {
                            val line = doc.getLineNumber(psiNav.offset)
                            val lineStartOffset = doc.getLineStartOffset(line)
                            val column = psiNav.offset - lineStartOffset
                            Position(line, column)
                        } else {
                            Position(0, 0)
                        }
                        Location(uri, Range(position, position))
                    }
                    else -> null
                }
            } else {
                null
            }
        }
        is SourcePosition -> {
            val uri = getURIForFile(nav.file)
            val position = Position(nav.line, nav.offset)
            Location(uri, Range(position, position))
        }
        else -> null
    }
}

fun getNavagatable(element: PsiElement): Navigatable? {
    val navigatable = if (element is Navigatable) element else EditSourceUtil.getDescriptor(element)
    if (navigatable != null && navigatable.canNavigate()) {
        return navigatable
    }
    return null
}

fun findTargetElements(project: Project, editor: Editor, file: PsiFile, position: Position): Array<PsiElement>? {
    val doc = getDocument(file) ?: return null

    val offset = positionToOffset(doc, position)

    val elements = GotoDeclarationAction.findAllTargetElements(project, editor, offset)

    if (elements.size != 1) {
        if (elements.isEmpty() && suggestionCandidates(TargetElementUtil.findReference(editor, offset)).isEmpty()) {
            val element = GotoDeclarationAction.findElementToShowUsagesOf(editor, editor.caretModel.offset)
            if (element != null) {
                return arrayOf(element)
            }

            //disable 'no declaration found' notification for keywords
            val elementAtCaret = file.findElementAt(offset)
            if (elementAtCaret != null) {
                val namesValidator = LanguageNamesValidation.INSTANCE.forLanguage(elementAtCaret.language)
                if (namesValidator != null && namesValidator.isKeyword(elementAtCaret.text, project)) {
                    return arrayOf(elementAtCaret)
                }
            }
        }
        return chooseAmbiguousTarget(editor, offset, elements)
    }

    val element = elements[0]
    if (element === GotoDeclarationAction.findElementToShowUsagesOf(editor, editor.caretModel.offset)
        && element != null) {
        return arrayOf(element)
    }

    var navElement: PsiElement? = element.navigationElement
    navElement = TargetElementUtil.getInstance().getGotoDeclarationTarget(element, navElement)
    if (navElement != null) {
        return arrayOf(navElement)
    }

    return null
}

fun chooseAmbiguousTarget(editor: Editor,
                          offset: Int,
                          elements: Array<PsiElement>?): Array<PsiElement>? {
    var elements = elements
    if (TargetElementUtil.inVirtualSpace(editor, offset)) {
        return null
    }

    val reference = TargetElementUtil.findReference(editor, offset)

    if (elements == null || elements.size == 0) {
        elements = if (reference == null)
            PsiElement.EMPTY_ARRAY
        else
            PsiUtilCore.toPsiElementArray(suggestionCandidates(reference))
    }

    if (elements!!.size == 1) {
        return elements
    }
    if (elements.size > 1) {
        if (reference != null) {
            val range = reference.rangeInElement
            val elementText = reference.element.text
            assert(range.startOffset >= 0 && range.endOffset <= elementText.length,
                { Arrays.toString(elements) + ";" + reference })
        }

        return elements
    }
    return null
}

private fun suggestionCandidates(reference: PsiReference?): Collection<PsiElement> {
    return if (reference == null) {
        emptyList()
    } else TargetElementUtil.getInstance().getTargetCandidates(reference)
}
