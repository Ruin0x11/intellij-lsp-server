package com.ruin.lsp.commands.project.symbol

import com.intellij.codeInsight.daemon.impl.DaemonProgressIndicator
import com.intellij.ide.actions.SearchEverywhereClassifier
import com.intellij.ide.actions.SearchEverywhereClassifier.EP_Manager.getProjectScope
import com.intellij.ide.util.gotoByName.*
import com.intellij.navigation.PsiElementNavigationItem
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.Document
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Ref
import com.intellij.openapi.util.registry.Registry
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.PsiClass
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiNameIdentifierOwner
import com.intellij.psi.impl.light.LightElement
import com.intellij.psi.search.ProjectScope
import com.ruin.lsp.commands.ProjectCommand
import com.ruin.lsp.commands.document.find.sourceLocationIfPossible
import com.ruin.lsp.util.*
import com.ruin.lsp.values.DocumentUri
import org.eclipse.lsp4j.SymbolInformation
import org.jetbrains.kotlin.asJava.classes.KtLightClass
import org.jetbrains.kotlin.asJava.elements.KtLightMethod
import org.jetbrains.kotlin.psi.KtClass
import org.jetbrains.kotlin.psi.KtFunction

const val MAX_SYMBOLS = 100
class WorkspaceSymbolCommand(val query: String) : ProjectCommand<MutableList<SymbolInformation>> {
    override fun execute(ctx: Project): MutableList<SymbolInformation> {
        val gotoSymbolModel = GotoSymbolModel2(ctx)
        val gotoClassModel = GotoClassModel2(ctx)

        val ref: Ref<List<SearchResult>> = Ref(listOf())
        ApplicationManager.getApplication().invokeAndWait {
            val symbols = getSymbols(ctx, query, MAX_SYMBOLS, false, NonInteractiveChooseByName(ctx, gotoSymbolModel, null))
            val classes = getClasses(ctx, query, MAX_SYMBOLS, false, NonInteractiveChooseByName(ctx, gotoClassModel, null))
            ref.set(listOf(symbols, classes))
        }

        val results = ref.get().flatMap { result ->
            result.mapNotNull {
                (it as PsiElement).toSymbolInformation()
            }.distinct()
        }

        return results.toMutableList()
    }
}

fun PsiElement.toSymbolInformation(): SymbolInformation? {
    if (this !is PsiNameIdentifierOwner) {
        return null
    }

    // these show up as duplicates alongside KtClass results
    if (this is KtLightClass) {
        return null
    }

    val name = this.symbolName() ?: return null
    val kind = this.symbolKind() ?: return null
    val location = this.sourceLocationIfPossible()
    val containerName =
        if (this.parent is PsiNameIdentifierOwner)
            (this.parent as PsiNameIdentifierOwner).name
        else null

    return SymbolInformation(name, kind, location, containerName)
}

class SearchResult : ArrayList<Any>() {
    var needMore: Boolean = false
}

/** copied from SearchEverywhereAction */
private fun getClasses(project: Project, pattern: String, max: Int, includeLibs: Boolean, chooseByNamePopup: ChooseByNameBase?): SearchResult {
    val classes = SearchResult()
    if (chooseByNamePopup == null || shouldSkipPattern(pattern)) {
        return classes
    }
    val scope = ProjectScope.getProjectScope(project)
    val myProgressIndicator = DaemonProgressIndicator()
    chooseByNamePopup.provider.filterElements(chooseByNamePopup, pattern, includeLibs,
        myProgressIndicator) { o ->
        if (SearchEverywhereClassifier.EP_Manager.isClass(o) && !classes.contains(o)) {
            if (classes.size == max) {
                classes.needMore = true
                return@filterElements false
            }
            val element = o as? PsiClass
            val virtualFile = SearchEverywhereClassifier.EP_Manager.getVirtualFile(o)
            val isElementWithoutFile = element != null && element.containingFile == null
            val isFileInScope = virtualFile != null && (includeLibs || scope?.accept(virtualFile))
            if (isElementWithoutFile || isFileInScope) {
                classes.add(o)
            }
        }
        true
    }
    return classes
}

/** copied from SearchEverywhereAction */
private fun getSymbols(project: Project, pattern: String, max: Int, includeLibs: Boolean, chooseByNamePopup: ChooseByNameBase): SearchResult {
    val symbols = SearchResult()
    if (!Registry.`is`("search.everywhere.symbols") || shouldSkipPattern(pattern)) {
        return symbols
    }
    val scope = ProjectScope.getProjectScope(project)
    val myProgressIndicator = DaemonProgressIndicator()
    chooseByNamePopup.provider.filterElements(chooseByNamePopup, pattern, includeLibs,
        myProgressIndicator) { o ->
        if (SearchEverywhereClassifier.EP_Manager.isSymbol(o) && !symbols.contains(o)) {
            var element: PsiElement? = null
            if (o is PsiElement) {
                element = o
            } else if (o is PsiElementNavigationItem) {
                element = o.targetElement
            }
            val virtualFile = SearchEverywhereClassifier.EP_Manager.getVirtualFile(o)
            //some elements are non-physical like DB columns
            val isElementWithoutFile = element != null && element.containingFile == null
            val isFileInScope = virtualFile != null && (includeLibs || scope?.accept(virtualFile) == true)
            val isSpecialElement = element == null && virtualFile == null //all Rider elements don't have any psi elements within
            if (isElementWithoutFile || isFileInScope || isSpecialElement) {
                symbols.add(o)
            }
        }
        symbols.needMore = symbols.size == max
        !symbols.needMore
    }
    return symbols
}

class NonInteractiveChooseByName(project: Project, model: ChooseByNameModel, context: PsiElement?)
    : ChooseByNameBase(project, model, ChooseByNameModelEx.getItemProvider(model, context), null) {
    override fun close(isOk: Boolean) {

    }

    override fun isShowListForEmptyPattern(): Boolean {
        return false
    }

    override fun hideList() {
    }

    override fun showList() {
    }

    override fun isCloseByFocusLost(): Boolean {
        return false
    }

    override fun isCheckboxVisible(): Boolean {
        return true
    }
}

private fun shouldSkipPattern(pattern: String): Boolean {
    return Registry.`is`("search.everywhere.pattern.checking") && StringUtil.split(pattern, ".").size == 2
}
