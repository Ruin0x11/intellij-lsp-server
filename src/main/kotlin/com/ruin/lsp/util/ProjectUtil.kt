package com.ruin.lsp.util

import com.intellij.openapi.Disposable
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.editor.LogicalPosition
import com.intellij.openapi.editor.ex.EditorEx
import com.intellij.openapi.editor.impl.DocumentImpl
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.fileEditor.impl.FileDocumentManagerImpl
import com.intellij.openapi.project.DumbService
import com.intellij.openapi.project.Project
import com.intellij.openapi.project.ex.ProjectManagerEx
import com.intellij.openapi.roots.OrderRootType
import com.intellij.openapi.roots.ProjectFileIndex
import com.intellij.openapi.util.Computable
import com.intellij.openapi.util.Disposer
import com.intellij.openapi.util.InvalidDataException
import com.intellij.openapi.util.Ref
import com.intellij.openapi.vfs.LocalFileSystem
import com.intellij.openapi.vfs.VfsUtilCore
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.openapi.vfs.VirtualFileManager
import com.intellij.openapi.wm.WindowManager
import com.intellij.openapi.wm.impl.WindowManagerImpl
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiFile
import com.intellij.psi.PsiManager
import com.intellij.psi.util.PsiUtilCore
import com.intellij.util.io.URLUtil
import com.ruin.lsp.model.MyLanguageClient
import com.ruin.lsp.model.MyLanguageServer
import com.ruin.lsp.values.DocumentUri
import org.eclipse.lsp4j.MessageParams
import org.eclipse.lsp4j.MessageType
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.services.LanguageClient
import org.jdom.JDOMException
import java.io.File
import java.io.IOException
import java.net.URI
import java.net.URLDecoder
import java.nio.file.Paths
import java.util.*

private val LOG = Logger.getInstance("#com.ruin.lsp.util.ProjectUtil")

private val POSSIBLE_SOURCE_EXTENSIONS = listOf(".java", ".kt", ".scala")

fun ensurePsiFromUri(project: Project, uri: DocumentUri, tempDir: DocumentUri? = null) = resolvePsiFromUri(project, uri, tempDir)
    ?: throw IllegalArgumentException("Unable to resolve file at $uri")

fun resolvePsiFromUri(project: Project, uri: DocumentUri, tempDir: DocumentUri? = null): PsiFile? {
    if (tempDir != null) {
        val normalizedTempDir = normalizeUri(tempDir)
        val prefix = normalizedTempDir.uriCommonPrefixWith(uri)
        if (prefix == normalizedTempDir) {
            return resolveJarUri(uri, normalizedTempDir)?.let { getJarVirtualFile(it) }?.let { getPsiFile(project, it) }
        }
    }
    val filePath = projectRelativeFilePath(project, uri) ?: return null
    return getPsiFile(project, filePath) ?: return null
}

/**
 * Given a project and a file URI, returns the path of the file relative to the project base path.
 */
fun projectRelativeFilePath(project: Project, uri: DocumentUri): String? {
    val newUri = normalizeUri(uri)
    val projPathUri = getURIForFile(File(project.basePath))
    val prefix = newUri.uriCommonPrefixWith(projPathUri) ?: return null
    return newUri.substring(prefix.length)
}

fun DocumentUri.uriCommonPrefixWith(other: DocumentUri): String? {
    val prefix = this.commonPrefixWith(other, true)

    return if (prefix.isEmpty() || prefix == "file:///") {
        null
    } else {
        prefix
    }
}

fun resolveProjectFromRootUri(uri: DocumentUri): Project? {
    // TODO: in-memory virtual files for testing have temp:/// prefix, figure out how to resolve the document from them
    // otherwise it gets confusing to have to look up the line and column being tested in the test document
    val newUri = normalizeUri(uri)
    val directory = File(uriToPath(newUri))
    if (!directory.isDirectory) {
        LOG.warn("root URI at $uri isn't a directory.")
        return null
    }
    return ensureProject(directory.absolutePath)
}

data class CachedProject(val project: Project, var disposable: Disposable? = null)

val sProjectCache = HashMap<String, CachedProject>()

internal class DumbModeNotifier(private val client: MyLanguageClient?,
                                private val server: MyLanguageServer?) : DumbService.DumbModeListener {
    override fun enteredDumbMode() {
        client?.notifyIndexStarted()
    }

    override fun exitDumbMode() {
        client?.notifyIndexFinished()
        server?.computeAllDiagnostics()
    }
}

fun registerIndexNotifier(project: Project, client: MyLanguageClient, server: MyLanguageServer) {
    val cached = sProjectCache.values.find { it.project == project } ?: return
    if (cached.disposable != null) {
        return
    }
    cached.disposable = Disposer.newDisposable()
    project.messageBus.connect(cached.disposable!!).subscribe(DumbService.DUMB_MODE, DumbModeNotifier(client, server))

    if (DumbService.isDumb(project)) {
        client.notifyIndexStarted()
    }
}

fun cacheProject(absolutePath: String, project: Project) {
    LOG.info("Caching project that was found at $absolutePath.")
    if (sProjectCache.containsKey(absolutePath)) {
        sProjectCache[absolutePath]!!.disposable?.dispose()
    }
    sProjectCache[absolutePath] = CachedProject(project)
}

fun ensureProject(projectPath: String): Project {
    val project = getProject(projectPath)
        ?: throw IllegalArgumentException("Couldn't find document at $projectPath")
    if (project.isDisposed)
        throw IllegalArgumentException("Project $project was already disposed!")

    return project
}

fun getProject(projectPath: String): Project? {
    val mgr = ProjectManagerEx.getInstanceEx()

    val cached = sProjectCache[projectPath]
    if (cached != null) {
        if (!cached.project.isDisposed) {
            return cached.project
        } else {
            LOG.info("Cached document at $projectPath was disposed, reopening.")
        }
    }

    try {
        if (!File(projectPath).exists()) {
            LOG.warn("Project at $projectPath doesn't exist.")
            return null
        }

        val projectRef = Ref<Project>()
        ApplicationManager.getApplication().runWriteAction {
            try {
                val alreadyOpenProject = mgr.openProjects.find {
                    uriToPath(it.baseDir.path)
                        .equals(projectPath.replace("\\", "/"), true)
                }

                val project = alreadyOpenProject ?: mgr.loadAndOpenProject(projectPath)
                projectRef.set(project)

                hideProjectFrame(project)
            } catch (e: IOException) {
                e.printStackTrace()
            } catch (e: JDOMException) {
                e.printStackTrace()
            } catch (e: InvalidDataException) {
                e.printStackTrace()
            }
        }

        val project = projectRef.get() ?: throw IOException("Failed to obtain document $projectPath")

        // Wait until the project is initialized to prevent invokeAndWait hangs
        while (!project.isInitialized) {
            Thread.sleep(1000)
        }

        cacheProject(projectPath, project)
        return project
    } catch (e: IOException) {
        e.printStackTrace()
    }

    LOG.warn("Exception occurred trying to find document for path $projectPath")
    return null
}

fun getPsiFile(project: Project, filePath: String): PsiFile? {
    return getPsiFile(project, getVirtualFile(project, filePath))
}

fun getPsiFile(project: Project, virtual: VirtualFile): PsiFile? {
    return invokeAndWaitIfNeeded(
        Computable<PsiFile> {
            val mgr = PsiManager.getInstance(project)
            val file = mgr.findFile(virtual)

            if (file == null) {
                LOG.warn("Unable to find PSI file for virtual file ${virtual.name}")
                return@Computable null
            }

            PsiUtilCore.ensureValid(file)
            file
        })
}

fun getVirtualFile(project: Project, filePath: String): VirtualFile {
    val projectDir = uriToPath(project.baseDir.toString())
    val file = File(projectDir, filePath)
    if (!file.exists()) {
        throw IllegalArgumentException("Couldn't find file $file")
    }

    // load the VirtualFile and ensure it's up to date
    val virtual = LocalFileSystem.getInstance()
        .refreshAndFindFileByIoFile(file)
    if (virtual == null || !virtual.exists()) {
        throw IllegalArgumentException("Couldn't locate virtual file @$file")
    }

    return virtual
}

fun getJarVirtualFile(jarUri: DocumentUri) = VirtualFileManager.getInstance().findFileByUrl(jarUri)

fun getDocument(project: Project, uri: String): Document? {
    val normalizedUri = normalizeUri(uri)
    val filePath = projectRelativeFilePath(project, normalizedUri) ?: return null
    val virtual = getVirtualFile(project, filePath)
    return invokeAndWaitIfNeeded(asWriteAction(
        Computable<Document> {
            val mgr = PsiManager.getInstance(project)
            val file = mgr.findFile(virtual)

            if (file == null) {
                LOG.warn("Unable to find PSI file for virtual file ${virtual.name}")
                return@Computable null
            }

            val doc = getDocument(file) ?: return@Computable null
            doc
        }))
}

fun getLibrarySourceVirtualFile(virtual: VirtualFile, index: ProjectFileIndex): VirtualFile? {
    val classRoot = index.getClassRootForFile(virtual) ?: return null
    val relativePath = VfsUtilCore.getRelativePath(virtual, classRoot) ?: return null

    searchForOtherSourceDirs@ for (entry in index.getOrderEntriesForFile(virtual)) {
        for (sourceRoot in entry.getFiles(OrderRootType.SOURCES)) {
            for (ext in POSSIBLE_SOURCE_EXTENSIONS) {
                val possibleSourceFilename = relativePath.replace("""\.class$""".toRegex(), ext)
                val sourceFile = sourceRoot.findFileByRelativePath(possibleSourceFilename)
                if (sourceFile != null) {
                    return sourceFile
                }
            }
        }
    }

    return null
}

fun getDocument(file: PsiFile): Document? {
    val virtual = file.virtualFile ?: return file.viewProvider.document

    var doc = FileDocumentManager.getInstance()
        .getDocument(virtual)
    if (doc == null) {
        FileDocumentManagerImpl.registerDocument(
            DocumentImpl(file.viewProvider.contents),
            virtual)
        doc = FileDocumentManager.getInstance()
            .getDocument(virtual)

        if (doc == null) {
            LOG.warn("Unable to find Document for virtual file ${virtual.name}")
            return null
        }
    }

    return doc
}

fun reloadDocument(doc: Document, project: Project) {
    FileDocumentManager.getInstance().reloadFromDisk(doc)
    PsiDocumentManager.getInstance(project).commitDocument(doc)
}

fun createEditor(context: Disposable, file: PsiFile, position: Position = Position(0, 0)): EditorEx {
    val doc = getDocument(file)!!
    val editorFactory = EditorFactory.getInstance()
    val created = editorFactory.createEditor(doc, file.project) as EditorEx
    created.caretModel.moveToLogicalPosition(LogicalPosition(position.line, position.character))

    Disposer.register(context, Disposable { editorFactory.releaseEditor(created) })

    return created
}

fun createEditor(context: Disposable, file: PsiFile, offset: Int): EditorEx {
    val doc = getDocument(file)!!
    val editorFactory = EditorFactory.getInstance()
    val created = editorFactory.createEditor(doc, file.project) as EditorEx
    created.caretModel.moveToOffset(offset)

    Disposer.register(context, Disposable { editorFactory.releaseEditor(created) })

    return created
}

/**
 * Gets a Windows-compatible URI from a VirtualFile.
 * The getPath() method of VirtualFile is missing an extra slash in the "file:///" protocol.
 */
fun getURIForFile(file: VirtualFile) = normalizeUri(file.url)

fun getURIForFile(file: PsiFile) = getURIForFile(file.virtualFile)

fun getURIForFile(file: File) = normalizeUri(file.toURI().toURL().toString())


/**
 * Converts URIs to have forward slashes and ensures the protocol has three slashes.
 *
 * Important for testing URIs for equality across platforms.
 */
fun normalizeUri(uri: String): String {
    val protocolRegex = "^file:/+".toRegex()
    val trailingSlashRegex = "/$".toRegex()
    var decodedUri = URLDecoder.decode(uri, "UTF-8")
    decodedUri = trailingSlashRegex.replace(decodedUri, "")
    decodedUri = protocolRegex.replace(decodedUri, "file:///")
    return decodedUri.replace("\\", "/")
}

/** Converts a URI to a path.
 *
 * Needed since trying to create a {@link java.net.URI} from an LSP-generated
 * URI string can fail, because Java's URI class does not accept unencoded
 * spaces.
 */
fun uriToPath(uri: String): String {
    val newUri = normalizeUri(URLDecoder.decode(uri, "UTF-8"))

    val isWindowsPath = """^file:/+\w:""".toRegex().containsMatchIn(newUri)

    return if (isWindowsPath)
        Paths.get("^file:/+".toRegex().replace(newUri, ""))
            .toString().replace("\\", "/")
    else {
        "^file:/+".toRegex().replace(newUri, "/")
    }
}

fun resolveJarUri(uri: DocumentUri, tempDirectory: DocumentUri): DocumentUri? {
    val pair = jarExtractedFileToJarpathFile(uri, tempDirectory) ?: return null
    val (internalSourceFile, jarpathFileUri) = pair

    val realJarPath = uriToPath(jarpathFileUri)
        .let { File(it) }.readText()

    return getJarEntryURI(realJarPath, internalSourceFile)
}

fun jarExtractedFileToJarpathFile(extractedFileUri: DocumentUri, tempDirectory: DocumentUri): Pair<String, DocumentUri>? {
    val normalizedUri = normalizeUri(extractedFileUri)
    val split = normalizedUri.substring(tempDirectory.length + "/".length)
        .split("/", limit = 3)
    if (split.size != 3) {
        return null
    }
    val (prefix /* "lsp-intellij" */, jarName, internalSourceFile) = split
    if (prefix != "lsp-intellij") {
        return null
    }
    val jarpathFileUri = "$tempDirectory/lsp-intellij/$jarName/jarpath"
    return Pair(internalSourceFile, jarpathFileUri)
}

fun getJarEntryURI(jarUri: DocumentUri, internalSourceFile: String): String? {
    val realJarFile = File(uriToPath(jarUri))
    if (!realJarFile.exists()) {
        return null
    }
    //val internalClassFile = internalSourceFile.replace(SOURCE_FILE_TO_CLASS_REGEX, CLASS_FILE_EXTENSION)
    return URLUtil.getJarEntryURL(realJarFile, internalSourceFile).toString()
        .replace("""^jar:file:/""".toRegex(), "jar://")
}


private fun hideProjectFrame(project: Project?) {
    val mgr = WindowManager.getInstance()
    val existing = mgr.getFrame(project)
    if (null != existing) {
        // hide any existing frames. We may want this
        //  to be a preference... Not sure
        existing.isVisible = false
        return // already done
    }

    if (mgr !is WindowManagerImpl) {
        // unit test?
        return
    }

    val impl = mgr.allocateFrame(project!!)
    impl.isVisible = false
}

fun toggleProjectFrame(project: Project) {
    val mgr = WindowManager.getInstance()
    val existing = mgr.getFrame(project)
    if (null != existing) {
        existing.isVisible = !existing.isVisible
    }
}

fun warnNoJdk(client: LanguageClient) {
    client.showMessage(MessageParams(MessageType.Warning,
        "Project SDK is not defined. Execute the command \"openProjectStructure\" to set it up."))
}
