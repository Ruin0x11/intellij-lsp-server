package com.ruin.lsp.commands.document.completion

import com.intellij.codeInsight.completion.*
import com.intellij.openapi.Disposable
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Caret
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.event.DocumentEvent
import com.intellij.openapi.editor.event.DocumentListener
import com.intellij.openapi.editor.impl.event.DocumentEventImpl
import com.intellij.openapi.project.DumbService
import com.intellij.openapi.util.Disposer
import com.intellij.openapi.util.Key
import com.intellij.openapi.util.Ref
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.impl.source.PsiFileImpl
import com.intellij.psi.impl.source.tree.injected.InjectedLanguageUtil
import com.intellij.psi.util.PsiModificationTracker
import com.ruin.lsp.util.createFileCopy
import java.lang.reflect.Constructor
import java.util.*

private val LOG = Logger.getInstance("#com.ruin.lsp.commands.completion.CompletionParametersUtil")
private var sConstructor: Constructor<CompletionParameters>? = null

fun newInstance(position: PsiElement?, originalFile: PsiFile,
                completionType: CompletionType, offset: Int, invocationCount: Int, editor: Editor): CompletionParameters? {

    try {

        val cached = sConstructor

        val ctor: Constructor<CompletionParameters>
        if (cached == null) {
            ctor = CompletionParameters::class.java.getDeclaredConstructor(
                PsiElement::class.java /* position */, PsiFile::class.java /* originalFile */,
                CompletionType::class.java, Int::class.javaPrimitiveType /* offset */, Int::class.javaPrimitiveType /* invocationCount */,
                Editor::class.java
            )
            ctor.isAccessible = true
            sConstructor = ctor
        } else {
            ctor = cached
        }

        return ctor.newInstance(position, originalFile, completionType, offset, invocationCount, editor)
    } catch (e: Throwable) {
        e.printStackTrace()
    }

    return null
}

fun makeCompletionParameters(editor: Editor, psiFile: PsiFile): CompletionParameters? {
    val initContext = makeInitContext(editor, psiFile, editor.caretModel.currentCaret)
    val (translator, hostCopyOffsets) = insertDummyIdentifier(initContext) ?: return null
    val newContext = createCompletionContext(psiFile, hostCopyOffsets)
    return makeCompletionParametersInternal(editor, 0, newContext)
}

private fun makeInitContext(editor: Editor, psiFile: PsiFile, caret: Caret): CompletionInitializationContext {
    val current = Ref.create<CompletionContributor>(null)
    val context = object : CompletionInitializationContext(editor, caret, psiFile, CompletionType.BASIC, 0) {
        internal var dummyIdentifierChanger: CompletionContributor? = null

        override fun setDummyIdentifier(dummyIdentifier: String) {
            super.setDummyIdentifier(dummyIdentifier)

            if (dummyIdentifierChanger != null) {
                LOG.error("Changing the dummy identifier twice, already changed by " + dummyIdentifierChanger!!)
            }
            dummyIdentifierChanger = current.get()
        }
    }
    val contributors = CompletionContributor.forLanguage(context.positionLanguage)
    val project = psiFile.project
    val filteredContributors = DumbService.getInstance(project).filterByDumbAwareness(contributors)
    for (contributor in filteredContributors) {
        current.set(contributor)
        contributor.beforeCompletion(context)
        //CompletionAssertions.checkEditorValid(editor)
        assert(!PsiDocumentManager.getInstance(project).isUncommited(editor.document)) { "Contributor $contributor left the document uncommitted" }
    }
    return context
}

private fun insertDummyIdentifier(initContext: CompletionInitializationContext): Pair<OffsetTranslator, OffsetsInFile>? {
    val hostEditor = InjectedLanguageUtil.getTopLevelEditor(initContext.editor)
    val topLevelOffsets = OffsetsInFile(initContext.file, initContext.offsetMap).toTopLevelFile()
    val hostMap = topLevelOffsets.offsets

    val hostCopy = createFileCopy(topLevelOffsets.file)
    val copyDocument = hostCopy.viewProvider.document ?: error("no document")
    val copyOffsets = topLevelOffsets.toFileCopy(hostCopy)
    val translator = OffsetTranslator(hostEditor.document, initContext.file, copyDocument)

    //CompletionAssertions.checkEditorValid(initContext.editor)
    val dummyIdentifier = initContext.dummyIdentifier
    if (!StringUtil.isEmpty(dummyIdentifier)) {
        val startOffset = hostMap.getOffset(CompletionInitializationContext.START_OFFSET)
        val endOffset = hostMap.getOffset(CompletionInitializationContext.SELECTION_END_OFFSET)
        copyDocument.replaceString(startOffset, endOffset, dummyIdentifier)
    }
    //CompletionAssertions.checkEditorValid(initContext.editor)

    val project = initContext.project

    PsiDocumentManager.getInstance(project).commitDocument(copyDocument)
    if (isAnythingInvalidatedAfterCommit(initContext, hostCopy)) {
        Disposer.dispose(translator)
        return null
    }

    return Pair(translator, copyOffsets)
}

private fun isAnythingInvalidatedAfterCommit(initContext: CompletionInitializationContext, hostCopy: PsiFile): Boolean {
    return !initContext.file.isValid || !hostCopy.isValid
        //|| !CompletionAssertions.isEditorValid(initContext.editor)
}


private fun makeCompletionParametersInternal(editor: Editor,
                                     invocationCount: Int,
                                     newContext: CompletionContext): CompletionParameters? {

    val offset = newContext.startOffset
    val fileCopy = newContext.file
    val originalFile = fileCopy.originalFile
    val insertedElement = findCompletionPositionLeaf(newContext, offset, fileCopy, originalFile)
    insertedElement.putUserData(CompletionContext.COMPLETION_CONTEXT_KEY, newContext)
    return newInstance(insertedElement, originalFile, CompletionType.BASIC, offset, invocationCount, editor)
}

private fun findCompletionPositionLeaf(newContext: CompletionContext, offset: Int, fileCopy: PsiFile, originalFile: PsiFile): PsiElement {
    val insertedElement = newContext.file.findElementAt(offset)
    //CompletionAssertions.assertCompletionPositionPsiConsistent(newContext, offset, fileCopy, originalFile, insertedElement!!)
    return insertedElement!!
}

private fun createCompletionContext(originalFile: PsiFile, hostCopyOffsets: OffsetsInFile): CompletionContext {
    //CompletionAssertions.assertHostInfo(hostCopyOffsets.file, hostCopyOffsets.offsets)

    val hostStartOffset = hostCopyOffsets.offsets.getOffset(CompletionInitializationContext.START_OFFSET)
    var result = hostCopyOffsets
    val translatedOffsets = hostCopyOffsets.toInjectedIfAny(hostStartOffset)
    if (translatedOffsets != hostCopyOffsets) {
        val injected = translatedOffsets.file
        if (injected is PsiFileImpl) {
            injected.originalFile = originalFile
        }
        val documentWindow = InjectedLanguageUtil.getDocumentWindow(injected)
        //CompletionAssertions.assertInjectedOffsets(hostStartOffset, injected, documentWindow)

        if (injected.textRange.contains(translatedOffsets.offsets.getOffset(CompletionInitializationContext.START_OFFSET))) {
            result = translatedOffsets
        }
    }

    return CompletionContext(result.file, result.offsets)
}

internal class OffsetTranslator(originalDocument: Document, private val myOriginalFile: PsiFile, private val myCopyDocument: Document) : Disposable {
    private val myTranslation = LinkedList<DocumentEvent>()

    private val isUpToDate: Boolean
        get() =
            this === myCopyDocument.getUserData(RANGE_TRANSLATION) && myOriginalFile.isValid

    init {
        myCopyDocument.putUserData(RANGE_TRANSLATION, this)
        Disposer.register(myOriginalFile.project, this)

        val sinceCommit = LinkedList<DocumentEvent>()
        originalDocument.addDocumentListener(object : DocumentListener {
            override fun documentChanged(e: DocumentEvent?) {
                if (isUpToDate) {
                    val inverse = DocumentEventImpl(originalDocument, e!!.offset, e.newFragment, e.oldFragment, 0, false)
                    sinceCommit.addLast(inverse)
                }
            }
        }, this)

        myCopyDocument.addDocumentListener(object : DocumentListener {
            override fun documentChanged(e: DocumentEvent?) {
                if (isUpToDate) {
                    myTranslation.addFirst(e)
                }
            }
        }, this)

        myOriginalFile.project.messageBus.connect(this).subscribe<PsiModificationTracker.Listener>(PsiModificationTracker.TOPIC, object : PsiModificationTracker.Listener {
            internal var lastModCount = myOriginalFile.getViewProvider().getModificationStamp()
            override fun modificationCountChanged() {
                if (isUpToDate && lastModCount != myOriginalFile.getViewProvider().getModificationStamp()) {
                    myTranslation.addAll(sinceCommit)
                    sinceCommit.clear()
                }
            }
        })

    }

    override fun dispose() {
        if (isUpToDate) {
            myCopyDocument.putUserData(RANGE_TRANSLATION, null)
        }
    }

    fun translateOffset(offset: Int?): Int? {
        var offset = offset
        for (event in myTranslation) {
            offset = translateOffset(offset!!, event)
            if (offset == null) {
                return null
            }
        }
        return offset
    }

    companion object {
        val RANGE_TRANSLATION = Key.create<OffsetTranslator>("completion.rangeTranslation")

        private fun translateOffset(offset: Int, event: DocumentEvent): Int? {
            if (event.offset < offset && offset < event.offset + event.newLength) {
                return if (event.oldLength == 0) {
                    event.offset
                } else null

            }

            return if (offset <= event.offset) offset else offset - event.newLength + event.oldLength
        }
    }
}
