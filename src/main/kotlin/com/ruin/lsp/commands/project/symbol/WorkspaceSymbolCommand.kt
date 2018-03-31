package com.ruin.lsp.commands.project.symbol

import com.intellij.codeInsight.daemon.impl.DaemonProgressIndicator
import com.intellij.ide.actions.SearchEverywhereClassifier
import com.intellij.ide.actions.SearchEverywhereClassifier.EP_Manager.getProjectScope
import com.intellij.ide.util.gotoByName.*
import com.intellij.navigation.PsiElementNavigationItem
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Ref
import com.intellij.openapi.util.registry.Registry
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiNameIdentifierOwner
import com.ruin.lsp.commands.ProjectCommand
import com.ruin.lsp.util.location
import com.ruin.lsp.util.symbolKind
import org.eclipse.lsp4j.SymbolInformation

const val MAX_SYMBOLS = 100
class WorkspaceSymbolCommand(val query: String) : ProjectCommand<MutableList<SymbolInformation>> {
    override fun execute(ctx: Project): MutableList<SymbolInformation> {
        val gotoSymbolModel = GotoSymbolModel2(ctx)
        val gotoClassModel = GotoClassModel2(ctx)

        val ref: Ref<List<SearchResult>> = Ref(listOf())
        ApplicationManager.getApplication().invokeAndWait {
            val symbols = getSymbols(ctx, query, MAX_SYMBOLS, true, NonInteractiveChooseByName(ctx, gotoSymbolModel, null))
            val classes = getClasses(query, MAX_SYMBOLS, true, NonInteractiveChooseByName(ctx, gotoClassModel, null))
            ref.set(listOf(symbols, classes))
        }

        val results = ref.get().flatMap { result ->
            result.mapNotNull {
                (it as PsiElement).toSymbolInformation()
            }
        }

        return results.toMutableList()
    }
}

fun PsiElement.toSymbolInformation(): SymbolInformation? {
    if (this !is PsiNameIdentifierOwner) {
        return null
    }

    val name = this.name!!
    val kind = this.symbolKind()!!
    val location = this.location()
    val containerName =
        if (this.parent is PsiNameIdentifierOwner)
            (this.parent as PsiNameIdentifierOwner).name
        else null

    return SymbolInformation(name, kind, location, containerName)
}

class SearchResult : ArrayList<Any>() {
    var needMore: Boolean = false
}

/** from SearchEverywhereAction */
private fun getClasses(pattern: String, max: Int, includeLibs: Boolean, chooseByNamePopup: ChooseByNameBase?): SearchResult {
    val classes = SearchResult()
    if (chooseByNamePopup == null || shouldSkipPattern(pattern)) {
        return classes
    }
    val myProgressIndicator = DaemonProgressIndicator()
    chooseByNamePopup.provider.filterElements(chooseByNamePopup, pattern, includeLibs,
        myProgressIndicator) { o ->
        if (SearchEverywhereClassifier.EP_Manager.isClass(o) && !classes.contains(o)) {
            if (classes.size == max) {
                classes.needMore = true
                return@filterElements false
            }
            classes.add(o)
        }
        true
    }
    return if (!includeLibs && classes.isEmpty()) {
        getClasses(pattern, max, true, chooseByNamePopup)
    } else classes
}

/** from SearchEverywhereAction */
private fun getSymbols(project: Project, pattern: String, max: Int, includeLibs: Boolean, chooseByNamePopup: ChooseByNameBase): SearchResult {
    val symbols = SearchResult()
    if (!Registry.`is`("search.everywhere.symbols") || shouldSkipPattern(pattern)) {
        return symbols
    }
    val scope = getProjectScope(project)
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

    return if (!includeLibs && symbols.isEmpty()) {
        getSymbols(project, pattern, max, true, chooseByNamePopup)
    } else symbols

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
