package com.ruin.intel.Util

import com.intellij.openapi.Disposable
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.editor.LogicalPosition
import com.intellij.openapi.editor.ex.EditorEx
import com.intellij.openapi.editor.impl.DocumentImpl
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.fileEditor.impl.FileDocumentManagerImpl
import com.intellij.openapi.project.Project
import com.intellij.openapi.project.ex.ProjectManagerEx
import com.intellij.openapi.util.Computable
import com.intellij.openapi.util.Disposer
import com.intellij.openapi.util.InvalidDataException
import com.intellij.openapi.util.Ref
import com.intellij.openapi.util.ThrowableComputable
import com.intellij.openapi.vfs.LocalFileSystem
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.openapi.wm.WindowManager
import com.intellij.openapi.wm.impl.WindowManagerImpl
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiFile
import com.intellij.psi.PsiManager
import com.intellij.psi.util.PsiUtilCore
import com.intellij.testFramework.fixtures.LightPlatformCodeInsightFixtureTestCase
import com.intellij.util.ui.UIUtil
import org.jdom.JDOMException
import org.jetbrains.annotations.NotNull
import java.io.File
import java.io.IOException
import java.net.URI
import java.util.*


fun resolvePsiFromUri(uri: String) : Pair<Project, PsiFile>? {
    val (project, filePath) = resolveProjectFromUri(uri) ?: return null
    val file = getPsiFile(project, filePath) ?: return null
    return Pair(project, file)
}

fun resolveProjectFromUri(uri: String) : Pair<Project, String>? {
    // TODO: in-memory virtual files for testing have temp:/// prefix, figure out how to resolve the project from them
    // otherwise it gets confusing to have to look up the line and column being tested in the test project

    // has to have three slashes on windows
    val newUri = uri.replace("file://", "file:///")
    val uri_b = URI(newUri)
    val topFile = File(uri_b)
    var directory = topFile.parentFile
    while (directory != null) {
        val project = directory.listFiles().firstOrNull { it.extension == "iml" }
        if (project != null) {
            val proj = ensureProject(project.absolutePath)
            val relativePath = directory.toPath().relativize(topFile.toPath())
            return Pair(proj, relativePath.toString())
        }
        directory = directory.parentFile
    }

    return null
}

val sProjectCache = HashMap<String, Project>()

fun ensureProject(projectPath: String): Project {
    val project = getProject(projectPath)
        ?: throw IllegalArgumentException("Couldn't find project at " + projectPath)
    if (project.isDisposed)
        throw IllegalArgumentException("Project $project was already disposed!")

    return project
}

fun getProject(projectPath: String): Project? {
    val mgr = ProjectManagerEx.getInstanceEx()

    val cached = sProjectCache[projectPath]
    if (cached != null && !cached.isDisposed) {
        return cached
    }

    try {
        if (!File(projectPath).exists())
            return null

        val projectRef = Ref<Project>()
        ApplicationManager.getApplication().runWriteAction {
            try {
                // NB: The line below causes a window to be opened for
                //  the project. But! RunCommand breaks in IntelliJ 14 without it.
                //  So, we now hide any existing frames in allocateFrame().
                //  I'm not sure if this is the right way to do it, but it doesn't
                //  seem to cause any problems so far....
                val project = mgr.loadAndOpenProject(projectPath)
                projectRef.set(project)

                //allocateFrame(project)
                //mockMessageView(project)
            } catch (e: IOException) {
                e.printStackTrace()
            } catch (e: JDOMException) {
                e.printStackTrace()
            } catch (e: InvalidDataException) {
                e.printStackTrace()
            }
        }

        val project = projectRef.get() ?: throw IOException("Failed to obtain project " + projectPath)

        sProjectCache[projectPath] = project
        return project
    } catch (e: IOException) {
        e.printStackTrace()
    }

    return null
}

fun getPsiFile(project: Project, filePath: String): PsiFile? {
    return getPsiFile(project, getVirtualFile(project, filePath))
}

fun getPsiFile(@NotNull project: Project, @NotNull virtual: VirtualFile): PsiFile? {
    return invokeAndWaitIfNeeded(asWriteAction(
            Computable<PsiFile> {
                val mgr = PsiManager.getInstance(project)
                val file = mgr.findFile(virtual) ?: return@Computable null

                var doc = FileDocumentManager.getInstance()
                        .getDocument(virtual)
                if (doc == null) {
                    FileDocumentManagerImpl.registerDocument(
                            DocumentImpl(file.viewProvider.contents),
                            virtual)
                    doc = FileDocumentManager.getInstance()
                            .getDocument(virtual)

                    doc ?: return@Computable null
                }

                FileDocumentManager.getInstance().reloadFromDisk(doc)
                PsiDocumentManager.getInstance(project).commitDocument(doc)

                mgr.reloadFromDisk(file)

                PsiUtilCore.ensureValid(file)
                file
            }))
}

fun getVirtualFile(project: Project, filePath: String): VirtualFile {
    val file = File(project.basePath, filePath)
    if (!file.exists()) {
        throw IllegalArgumentException("Couldn't find file " + file)
    }

    // load the VirtualFile and ensure it's up to date
    val virtual = LocalFileSystem.getInstance()
            .refreshAndFindFileByIoFile(file)
    if (virtual == null || !virtual.exists()) {
        throw IllegalArgumentException("Couldn't locate virtual file @" + file)
    }
    LocalFileSystem.getInstance().refreshFiles(Collections.singletonList(virtual))

    return virtual
}

fun getDocument(file: PsiFile) =
    FileDocumentManager.getInstance()
            .getDocument(file.virtualFile)!!

fun createEditor(context: Disposable, file: PsiFile, line: Int, column: Int) : EditorEx {
    val doc = getDocument(file)
    val editorFactory = EditorFactory.getInstance()
    val created = editorFactory.createEditor(doc, file.project) as EditorEx
    created.caretModel.moveToLogicalPosition(LogicalPosition(line, column))

    Disposer.register(context, Disposable { editorFactory.releaseEditor(created) })

    return created
}

private fun allocateFrame(project: Project?) {
        val mgr = WindowManager.getInstance();
        val existing = mgr.getFrame(project);
        if (null != existing) {
            // hide any existing frames. We may want this
            //  to be a preference... Not sure
            existing.isVisible = false;
            return // already done
        }

        if (mgr !is WindowManagerImpl) {
            // unit test?
            return
        }

        val impl = mgr.allocateFrame(project!!)
        impl.isVisible = false
    }
