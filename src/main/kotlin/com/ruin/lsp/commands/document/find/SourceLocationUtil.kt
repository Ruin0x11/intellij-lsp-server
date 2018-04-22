package com.ruin.lsp.commands.document.find

import com.intellij.openapi.editor.ScrollType
import com.intellij.openapi.extensions.Extensions
import com.intellij.openapi.fileEditor.FileEditor
import com.intellij.openapi.fileEditor.FileEditorProvider
import com.intellij.openapi.fileEditor.ex.FileEditorManagerEx
import com.intellij.openapi.fileEditor.ex.FileEditorProviderManager
import com.intellij.openapi.fileEditor.impl.EditorFileSwapper
import com.intellij.openapi.fileEditor.impl.EditorHistoryManager
import com.intellij.openapi.fileEditor.impl.EditorWithProviderComposite
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiElement
import com.ruin.lsp.util.*
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.Range
import java.lang.reflect.Constructor

private var sConstructor: Constructor<EditorWithProviderComposite>? = null

internal fun newEditorCompositeInstance(file: VirtualFile, editors: Array<FileEditor?>, providers: Array<FileEditorProvider>, fileEditorManager: FileEditorManagerEx)
    : EditorWithProviderComposite? {

    try {
        val cached = sConstructor

        val ctor: Constructor<EditorWithProviderComposite>
        if (cached == null) {
            ctor = EditorWithProviderComposite::class.java.getDeclaredConstructor(
                VirtualFile::class.java /* file */,
                Array<FileEditor>::class.java /* editors */,
                Array<FileEditorProvider>::class.java /* providers */,
                FileEditorManagerEx::class.java /* fileEditorManager */
            )
            ctor.isAccessible = true
            sConstructor = ctor
        } else {
            ctor = cached
        }

        return ctor.newInstance(file, editors, providers, fileEditorManager)
    } catch (e: Throwable) {
        e.printStackTrace()
    }

    return null
}

internal fun newEditorComposite(file: VirtualFile?, project: Project): EditorWithProviderComposite? {
    if (file == null) {
        return null
    }

    val editorProviderManager = FileEditorProviderManager.getInstance()
    val providers = editorProviderManager.getProviders(project, file)
    if (providers.isEmpty()) return null
    val editors = arrayOfNulls<FileEditor>(providers.size)
    for (i in providers.indices) {
        val provider = providers[i]!!
        assert(provider.accept(project, file))
        val editor = provider.createEditor(project, file)
        editors[i] = editor
        assert(editor.isValid)
    }

    val fileEditorManager = FileEditorManagerEx.getInstance(project) as FileEditorManagerEx
    val newComposite = newEditorCompositeInstance(file, editors, providers, fileEditorManager)
    val editorHistoryManager = EditorHistoryManager.getInstance(project)
    for (i in editors.indices) {
        val editor = editors[i]!!

        val provider = providers[i]

        // Restore myEditor state
        val state = editorHistoryManager.getState(file, provider)
        if (state != null) {
            editor.setState(state)
        }
    }
    return newComposite
}

/**
 * Tries to find the corresponding source file location for this element.
 *
 * Depends on the element being contained in a library's class file and there being a corresponding Sources JAR attached
 * to the library.
 */
fun PsiElement.sourceLocationIfPossible(): Location {
    val doc = getDocument(this.containingFile)!!
    val uri = getURIForFile(this.containingFile)
    val location = this.location(uri, doc)

    val editor = newEditorComposite(this.containingFile.virtualFile, this.project)
        ?: return location
    val swappers = Extensions.getExtensions(EditorFileSwapper.EP_NAME)
    var newFilePair: com.intellij.openapi.util.Pair<VirtualFile, Int>? = null
    val psiAwareEditor = EditorFileSwapper.findSinglePsiAwareEditor(editor.editors) ?: return location
    psiAwareEditor.editor.caretModel.moveToOffset(location.range.start.toOffset(doc))
    psiAwareEditor.editor.scrollingModel.scrollToCaret(ScrollType.CENTER)

    swappers.forEach { each ->
        newFilePair = each.getFileToSwapTo(this.project, editor)
        if (newFilePair != null) return@forEach
    }

    if (newFilePair == null || newFilePair?.first == null) {
        return location
    }

    val sourcePsiFile = getPsiFile(this.project, newFilePair!!.first) ?: return location
    val sourceDoc = getDocument(sourcePsiFile) ?: return location
    val offset = newFilePair!!.second ?: 0
    location.range = Range(offsetToPosition(sourceDoc, offset), offsetToPosition(sourceDoc, offset))

    return location
}
