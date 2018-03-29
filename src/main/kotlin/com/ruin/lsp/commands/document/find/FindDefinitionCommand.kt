package com.ruin.lsp.commands.document.find

import com.intellij.ide.highlighter.JavaFileType
import com.intellij.openapi.editor.Document
import com.intellij.psi.PsiMethod
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.search.ProjectScope.getProjectScope
import com.intellij.psi.search.ProjectScopeImpl
import com.intellij.psi.search.SearchScope
import com.intellij.psi.search.searches.DefinitionsScopedSearch
import com.intellij.psi.search.searches.SuperMethodsSearch
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.model.LanguageServerException
import com.ruin.lsp.util.getDocument
import com.ruin.lsp.util.location
import com.ruin.lsp.util.toOffset
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.Position
import org.jetbrains.kotlin.backend.common.push
import org.jetbrains.kotlin.idea.KotlinFileType
import org.jetbrains.kotlin.idea.references.KtReference
import org.jetbrains.kotlin.idea.search.ideaExtensions.KotlinDefinitionsSearcher
import org.jetbrains.kotlin.resolve.lazy.NoDescriptorForDeclarationException

class FindDefinitionCommand(val position: Position) : DocumentCommand<MutableList<Location>> {
    override fun execute(ctx: ExecutionContext): MutableList<Location> {
        val doc = getDocument(ctx.file)
            ?: throw LanguageServerException("No document found.")

        val offset = position.toOffset(doc)

        return when {
            ctx.file.fileType is KotlinFileType -> executeForKotlin(ctx, doc, offset)
            ctx.file.fileType is JavaFileType -> executeForJava(ctx, doc, offset)
            else -> mutableListOf()
        }
    }

    private fun executeForJava(ctx: ExecutionContext, doc: Document, offset: Int): MutableList<Location> {
        val ref = ctx.file.findReferenceAt(offset)

        var lookup = ref?.resolve()

        if (lookup == null) {
            val element = ctx.file.findElementAt(offset)
            val parent = element?.parent
            if (parent != null && parent is PsiMethod) {
                val superSignature =
                    SuperMethodsSearch.search(parent, null, true, false).findFirst()
                lookup = superSignature?.method
            }
        }

        return lookup?.location()?.let { mutableListOf(it) }
            ?: mutableListOf()
    }

    private fun executeForKotlin(ctx: ExecutionContext, doc: Document, offset: Int): MutableList<Location> {
        val results = mutableListOf<Location>()
        try {
            val el = ctx.file.findElementAt(offset)
            val ref = ctx.file.findReferenceAt(offset) as? KtReference
            val elt = ref?.resolve() ?: return mutableListOf()
            KotlinDefinitionsSearcher().execute(DefinitionsScopedSearch.SearchParameters(elt, getProjectScope(ctx.project), true)) {
                results.push(it.location())
            }
        } catch (e: NoDescriptorForDeclarationException) {
            return mutableListOf()
        }
        return results
    }
}
