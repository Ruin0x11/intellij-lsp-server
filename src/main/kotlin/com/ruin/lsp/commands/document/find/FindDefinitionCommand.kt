package com.ruin.lsp.commands.document.find

import com.intellij.psi.PsiMethod
import com.intellij.psi.search.searches.SuperMethodsSearch
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.model.LanguageServerException
import com.ruin.lsp.util.getDocument
import com.ruin.lsp.util.location
import com.ruin.lsp.util.toOffset
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.Position

class FindDefinitionCommand(val position: Position) : DocumentCommand<MutableList<Location>> {
    override fun execute(ctx: ExecutionContext): MutableList<Location> {
        val doc = getDocument(ctx.file)
            ?: throw LanguageServerException("No document found.")

        val offset = position.toOffset(doc)
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

        return if (lookup != null) {
            mutableListOf(lookup.location())
        } else {
            mutableListOf()
        }
    }
}
