package com.ruin.lsp.commands.document.completion

import com.intellij.codeInsight.completion.*
import com.intellij.diagnostic.LogEventException
import com.intellij.injected.editor.DocumentWindow
import com.intellij.injected.editor.EditorWindow
import com.intellij.lang.injection.InjectedLanguageManager
import com.intellij.openapi.Disposable
import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.diagnostic.Attachment
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Caret
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.event.DocumentEvent
import com.intellij.openapi.editor.event.DocumentListener
import com.intellij.openapi.editor.impl.event.DocumentEventImpl
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.progress.util.AbstractProgressIndicatorExBase
import com.intellij.openapi.project.DumbService
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Disposer
import com.intellij.openapi.util.Key
import com.intellij.openapi.util.Ref
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.impl.DebugUtil
import com.intellij.psi.impl.PsiFileEx
import com.intellij.psi.impl.source.PsiFileImpl
import com.intellij.psi.impl.source.tree.injected.InjectedLanguageUtil
import com.intellij.psi.util.PsiModificationTracker
import com.intellij.psi.util.PsiUtilBase
import com.intellij.psi.util.PsiUtilCore
import com.ruin.lsp.util.obtainFileCopy
import org.jetbrains.annotations.Contract
import java.lang.reflect.Constructor
import java.util.*
import java.util.function.Supplier

private val LOG = Logger.getInstance("#com.ruin.lsp.commands.completion.CompletionParametersUtil")
private var completionParametersCtor: Constructor<CompletionParameters>? = null
private var offsetTranslatorCtor: Constructor<OffsetTranslator>? = null

fun newCompletionParametersInstance(position: PsiElement?, originalFile: PsiFile,
                                    completionType: CompletionType, offset: Int, invocationCount: Int, editor: Editor,
                                    indicator: CompletionProcess): CompletionParameters? {

    try {

        val cached = completionParametersCtor

        val ctor: Constructor<CompletionParameters>
        if (cached == null) {
            ctor = CompletionParameters::class.java.getDeclaredConstructor(
                PsiElement::class.java /* position */, PsiFile::class.java /* originalFile */,
                CompletionType::class.java, Int::class.javaPrimitiveType /* offset */, Int::class.javaPrimitiveType /* invocationCount */,
                Editor::class.java, CompletionProcess::class.java
            )
            ctor.isAccessible = true
            completionParametersCtor = ctor
        } else {
            ctor = cached
        }

        return ctor.newInstance(position, originalFile, completionType, offset, invocationCount, editor, indicator)
    } catch (e: Throwable) {
        e.printStackTrace()
    }

    return null
}

internal class VoidCompletionProcess : AbstractProgressIndicatorExBase(), Disposable, CompletionProcess {
    override fun isAutopopupCompletion() = false
    private val myLock = "VoidCompletionProcess"

    override fun dispose() {}

    fun registerChildDisposable(child: Supplier<Disposable>) {
        synchronized(myLock) {
            // avoid registering stuff on an indicator being disposed concurrently
            checkCanceled()
            Disposer.register(this, child.get())
        }
    }
}

fun makeCompletionParameters(editor: Editor, project: Project): CompletionParameters? {
    val ref = Ref<PsiFile?>()
    CommandProcessor.getInstance().runUndoTransparentAction {
        PsiDocumentManager.getInstance(project).commitAllDocuments()
        checkEditorValid(editor)

        val psiFile = PsiUtilBase.getPsiFileInEditor(editor.caretModel.currentCaret, project)
            ?: error("no PSI file: " + FileDocumentManager.getInstance().getFile(editor.document)!!)
        psiFile.putUserData(PsiFileEx.BATCH_REFERENCE_PROCESSING, java.lang.Boolean.TRUE)
        assertCommitSuccessful(editor, psiFile!!)
        ref.set(psiFile)
    }
    val psiFile = ref.get() ?: return null
    val process = VoidCompletionProcess()
    val initContext = makeInitContext(editor, psiFile, editor.caretModel.currentCaret)
    val topLevelOffsets = OffsetsInFile(initContext.file, initContext.offsetMap).toTopLevelFile()

    val hostCopyOffsets = insertDummyIdentifier(initContext, process, topLevelOffsets) ?: return null

    process.registerChildDisposable(Supplier { hostCopyOffsets.offsets })
    val finalOffsets = toInjectedIfAny(initContext.file, hostCopyOffsets)
    process.registerChildDisposable(Supplier { finalOffsets.offsets })

    val newContext = createCompletionContext(psiFile, hostCopyOffsets)
    return makeCompletionParametersInternal(editor, 0, newContext, process, finalOffsets)
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
    val project = psiFile.project
    val contributors = CompletionContributor.forLanguageHonorDumbness(context.positionLanguage, project)
    for (contributor in contributors) {
        current.set(contributor)
        contributor.beforeCompletion(context)
        checkEditorValid(editor)
        assert(!PsiDocumentManager.getInstance(project).isUncommited(editor.document)) { "Contributor $contributor left the document uncommitted" }
    }
    return context
}

private fun toInjectedIfAny(originalFile: PsiFile, hostCopyOffsets: OffsetsInFile): OffsetsInFile {
    assertHostInfo(hostCopyOffsets.file, hostCopyOffsets.offsets)

    val hostStartOffset = hostCopyOffsets.offsets.getOffset(CompletionInitializationContext.START_OFFSET)
    val translatedOffsets = hostCopyOffsets.toInjectedIfAny(hostStartOffset)
    if (translatedOffsets != hostCopyOffsets) {
        val injected = translatedOffsets.file
        if (injected is PsiFileImpl) {
            injected.originalFile = originalFile
        }
        val documentWindow = InjectedLanguageUtil.getDocumentWindow(injected)
        assertInjectedOffsets(hostStartOffset, injected, documentWindow)

        if (injected.textRange.contains(translatedOffsets.offsets.getOffset(CompletionInitializationContext.START_OFFSET))) {
            return translatedOffsets
        }
    }

    return hostCopyOffsets
}

private fun insertDummyIdentifier(initContext: CompletionInitializationContext,
                                  indicator: VoidCompletionProcess,
                                  topLevelOffsets: OffsetsInFile): OffsetsInFile? {
    val hostEditor = InjectedLanguageUtil.getTopLevelEditor(initContext.editor)
    val hostMap = topLevelOffsets.offsets

    val hostCopy = obtainFileCopy(topLevelOffsets.file)
    val copyDocument = hostCopy.viewProvider.document!!

    val dummyIdentifier = initContext.dummyIdentifier
    val startOffset = hostMap.getOffset(CompletionInitializationContext.START_OFFSET)
    val endOffset = hostMap.getOffset(CompletionInitializationContext.SELECTION_END_OFFSET)

    indicator.registerChildDisposable(
        Supplier { OffsetTranslator(hostEditor.document, initContext.file, copyDocument, startOffset, endOffset, dummyIdentifier)!! })

    val copyOffsets = topLevelOffsets.replaceInCopy(hostCopy, startOffset, endOffset, dummyIdentifier)
    return if (hostCopy.isValid) copyOffsets else null
}

private fun makeCompletionParametersInternal(editor: Editor,
                                             invocationCount: Int,
                                             newContext: CompletionContext,
                                             indicator: CompletionProcess,
                                             finalOffsets: OffsetsInFile): CompletionParameters? {

    val offset = newContext.startOffset
    val fileCopy = newContext.file
    val originalFile = fileCopy.originalFile
    val insertedElement = findCompletionPositionLeaf(finalOffsets, newContext, offset, fileCopy, originalFile)
    insertedElement.putUserData(CompletionContext.COMPLETION_CONTEXT_KEY, newContext)
    return newCompletionParametersInstance(insertedElement, originalFile, CompletionType.BASIC, offset, invocationCount, editor, indicator)
}

private fun findCompletionPositionLeaf(offsets: OffsetsInFile, newContext: CompletionContext, offset: Int, fileCopy: PsiFile, originalFile: PsiFile): PsiElement {
    val insertedElement = newContext.file.findElementAt(offset)
    assertCompletionPositionPsiConsistent(offsets, offset, originalFile, insertedElement)
    return insertedElement!!
}

private fun createCompletionContext(originalFile: PsiFile, hostCopyOffsets: OffsetsInFile): CompletionContext {
    assertHostInfo(hostCopyOffsets.file, hostCopyOffsets.offsets)

    val hostStartOffset = hostCopyOffsets.offsets.getOffset(CompletionInitializationContext.START_OFFSET)
    var result = hostCopyOffsets
    val translatedOffsets = hostCopyOffsets.toInjectedIfAny(hostStartOffset)
    if (translatedOffsets != hostCopyOffsets) {
        val injected = translatedOffsets.file
        if (injected is PsiFileImpl) {
            injected.originalFile = originalFile
        }
        val documentWindow = InjectedLanguageUtil.getDocumentWindow(injected)
        assertInjectedOffsets(hostStartOffset, injected, documentWindow)

        if (injected.textRange.contains(translatedOffsets.offsets.getOffset(CompletionInitializationContext.START_OFFSET))) {
            result = translatedOffsets
        }
    }

    return CompletionContext(result.file, result.offsets)
}

// copied since it's internal, unfortunately.
internal class OffsetTranslator(originalDocument: Document, private val myOriginalFile: PsiFile, private val myCopyDocument: Document, start: Int, end: Int, replacement: String) : Disposable {
    private val myTranslation = LinkedList<DocumentEvent>()

    private val isUpToDate: Boolean
        get() = this === myCopyDocument.getUserData(RANGE_TRANSLATION) && myOriginalFile.isValid

    init {
        myCopyDocument.putUserData(RANGE_TRANSLATION, this)
        myTranslation.addFirst(DocumentEventImpl(myCopyDocument, start, originalDocument.immutableCharSequence.subSequence(start, end), replacement, 0, false))
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

        myOriginalFile.project.messageBus.connect(this).subscribe(PsiModificationTracker.TOPIC, object : PsiModificationTracker.Listener {
            internal var lastModCount = myOriginalFile.viewProvider.modificationStamp
            override fun modificationCountChanged() {
                if (isUpToDate && lastModCount != myOriginalFile.viewProvider.modificationStamp) {
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


// copied from CompletionAssertions

fun assertCommitSuccessful(editor: Editor, psiFile: PsiFile) {
    val document = editor.document
    val docLength = document.textLength
    val psiLength = psiFile.textLength
    val manager = PsiDocumentManager.getInstance(psiFile.project)
    val committed = !manager.isUncommited(document)
    if (docLength == psiLength && committed) {
        return
    }

    val viewProvider = psiFile.viewProvider

    var message = "unsuccessful commit:"
    message += "\nmatching=" + (psiFile === manager.getPsiFile(document))
    message += "\ninjectedEditor=" + (editor is EditorWindow)
    message += "\ninjectedFile=" + InjectedLanguageManager.getInstance(psiFile.project).isInjectedFragment(psiFile)
    message += "\ncommitted=$committed"
    message += "\nfile=" + psiFile.name
    message += "\nfile class=" + psiFile.javaClass
    message += "\nfile.valid=" + psiFile.isValid
    message += "\nfile.physical=" + psiFile.isPhysical
    message += "\nfile.eventSystemEnabled=" + viewProvider.isEventSystemEnabled
    message += "\nlanguage=" + psiFile.language
    message += "\ndoc.length=$docLength"
    message += "\npsiFile.length=$psiLength"
    val fileText = psiFile.text
    if (fileText != null) {
        message += "\npsiFile.text.length=" + fileText.length
    }
    val node = psiFile.node
    if (node != null) {
        message += "\nnode.length=" + node.textLength
        val nodeText = node.text
        message += "\nnode.text.length=" + nodeText.length
    }
    val virtualFile = viewProvider.virtualFile
    message += "\nvirtualFile=$virtualFile"
    message += "\nvirtualFile.class=" + virtualFile.javaClass
    message += "\n" + DebugUtil.currentStackTrace()

    throw LogEventException("Commit unsuccessful", message,
        Attachment(virtualFile.path + "_file.txt", StringUtil.notNullize(fileText)),
        createAstAttachment(psiFile, psiFile),
        Attachment("docText.txt", document.text))
}

fun checkEditorValid(editor: Editor) {
    if (!isEditorValid(editor)) {
        throw AssertionError()
    }
}

fun isEditorValid(editor: Editor): Boolean {
    return editor !is EditorWindow || editor.isValid
}

fun assertInjectedOffsets(hostStartOffset: Int, injected: PsiFile, documentWindow: DocumentWindow?) {
    assert(documentWindow != null) { "no DocumentWindow for an injected fragment" }

    val host = InjectedLanguageManager.getInstance(injected.project).injectedToHost(injected, injected.textRange)
    assert(hostStartOffset >= host.startOffset) { "startOffset before injected" }
    assert(hostStartOffset <= host.endOffset) { "startOffset after injected" }
}

fun assertHostInfo(hostCopy: PsiFile, hostMap: OffsetMap) {
    PsiUtilCore.ensureValid(hostCopy)
    if (hostMap.getOffset(CompletionInitializationContext.START_OFFSET) >= hostCopy.textLength) {
        throw AssertionError("startOffset outside the host file: " + hostMap.getOffset(CompletionInitializationContext.START_OFFSET) + "; " + hostCopy)
    }
}

@Contract("_,_,_,null->fail")
fun assertCompletionPositionPsiConsistent(offsets: OffsetsInFile,
                                          offset: Int,
                                          originalFile: PsiFile, insertedElement: PsiElement?) {
    val fileCopy = offsets.file
    if (insertedElement == null) {
        throw LogEventException("No element at insertion offset",
            "offset=" +
                offset +
                "\n" +
                DebugUtil.currentStackTrace(),
            createFileTextAttachment(fileCopy, originalFile),
            createAstAttachment(fileCopy, originalFile))
    }

    if (fileCopy.findElementAt(offset) !== insertedElement) {
        throw AssertionError("wrong offset")
    }

    val range = insertedElement.textRange
    val fileCopyText = fileCopy.viewProvider.contents
    if (range.endOffset > fileCopyText.length || fileCopyText.subSequence(range.startOffset, range.endOffset).toString() != insertedElement.text) {
        throw LogEventException("Inconsistent completion tree", "range=" + range + "\n" + DebugUtil.currentStackTrace(),
            createFileTextAttachment(fileCopy, originalFile), createAstAttachment(fileCopy, originalFile),
            Attachment("Element at caret.txt", insertedElement.text))
    }
}

private fun createAstAttachment(fileCopy: PsiFile, originalFile: PsiFile): Attachment {
    return Attachment(originalFile.viewProvider.virtualFile.path + " syntactic tree.txt", DebugUtil.psiToString(fileCopy, false, true))
}

private fun createFileTextAttachment(fileCopy: PsiFile, originalFile: PsiFile): Attachment {
    return Attachment(originalFile.viewProvider.virtualFile.path, fileCopy.text)
}
