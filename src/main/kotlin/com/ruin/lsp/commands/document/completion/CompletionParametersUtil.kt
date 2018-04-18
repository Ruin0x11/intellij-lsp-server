package com.ruin.lsp.commands.document.completion

import com.intellij.codeInsight.completion.*
import com.intellij.lang.injection.InjectedLanguageManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.util.DocumentUtil
import com.ruin.lsp.util.createFileCopy
import com.ruin.lsp.util.runUndoTransparentWriteCommand
import com.ruin.lsp.util.toOffset
import org.eclipse.lsp4j.Position
import java.io.IOException
import java.lang.reflect.Constructor

private val LOG = Logger.getInstance("#com.ruin.lsp.commands.completion.CompletionParametersUtil")
var sConstructor: Constructor<CompletionParameters>? = null

fun newInstance(position: PsiElement?, originalFile: PsiFile,
                completionType: CompletionType, offset: Int, invocationCount: Int, editor: Editor): CompletionParameters? {

    try {

        val cached = sConstructor

        val ctor: Constructor<CompletionParameters>
        if (cached == null) {
            ctor = CompletionParameters::class.java.getDeclaredConstructor(
                PsiElement::class.java /* position */, PsiFile::class.java /* originalFile */,
                CompletionType::class.java, Int::class.javaPrimitiveType /* offset */, Int::class.javaPrimitiveType /* invocationCount */,
                Editor::class.java, CompletionProcess::class.java
            )
            ctor.isAccessible = true
            sConstructor = ctor
        } else {
            ctor = cached
        }

        return ctor.newInstance(position, originalFile, completionType, offset, invocationCount, editor, VoidCompletionProcess())
    } catch (e: Throwable) {
        e.printStackTrace()
    }

    return null
}
fun makeCompletionParameters(editor: Editor, psiFile: PsiFile, position: Position): CompletionParameters? {
    val offset = position.toOffset(editor.document)
    val elemAtPos = psiFile.findElementAt(offset)

    val completionType = CompletionType.BASIC

    if (elemAtPos == null) {
        LOG.warn("Couldn't find element at " + offset)
        LOG.warn("psif=" + psiFile.text)
        try {
            LOG.warn("file=" + String(psiFile.virtualFile.contentsToByteArray()))
        } catch (e: IOException) {
            e.printStackTrace()
        }

    }

    val invocationCount = 0

    val offsetMap = OffsetMap(editor.document)
    val context = CompletionContext(psiFile, offsetMap)
    elemAtPos!!.putUserData(CompletionContext.COMPLETION_CONTEXT_KEY, context)

    // we need to insert a dummy identifier so there's something there.
    // this is what intellij does typically
    val completionPosition = insertDummyIdentifier(
        psiFile, elemAtPos)

    return newInstance(completionPosition, psiFile,
        completionType, offset, invocationCount, editor)
}

/** based on CodeCompletionHandlerBase  */
private fun insertDummyIdentifier(originalFile: PsiFile,
                                  position: PsiElement): PsiElement? {
    val manager = InjectedLanguageManager
        .getInstance(originalFile.project)
    val hostFile = manager.getTopLevelFile(originalFile)

    var hostCopy = arrayOf<PsiFile>()
    DocumentUtil.writeInRunUndoTransparentAction(Runnable {
        val start = position.textOffset.toLong()
        val end = start + position.textLength.toLong()
        val copiedFile = createFileCopy(hostFile, start, end)
        hostCopy = arrayOf(copiedFile)
    })

    val copyDocument = hostCopy[0].viewProvider.document ?: throw IllegalStateException("No document found for copy")

    runUndoTransparentWriteCommand(Runnable {
        val dummyIdentifier = CompletionInitializationContext.DUMMY_IDENTIFIER_TRIMMED
        if (StringUtil.isEmpty(dummyIdentifier)) return@Runnable

        val startOffset = position.textOffset
        val endOffset = startOffset + position.textLength
        copyDocument.replaceString(startOffset, endOffset, dummyIdentifier)
    })

    PsiDocumentManager.getInstance(originalFile.project)
        .commitDocument(copyDocument)
    return hostCopy[0].findElementAt(position.textOffset)
}

internal class VoidCompletionProcess : CompletionProcess {
    override fun isAutopopupCompletion() = true
}
