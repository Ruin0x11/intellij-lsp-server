package com.ruin.lsp;

import com.intellij.JavaTestUtil
import com.intellij.compiler.CompilerTestUtil
import com.intellij.execution.RunManager
import com.intellij.execution.RunnerAndConfigurationSettings
import com.intellij.execution.application.ApplicationConfiguration
import com.intellij.execution.junit.JUnitConfiguration
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ModuleRootModificationUtil
import com.intellij.openapi.util.Disposer
import com.intellij.util.ui.UIUtil
import com.ruin.lsp.util.asWriteAction
import com.ruin.lsp.util.ensureProject
import junit.framework.TestCase
import java.io.IOException


/**
 * Wacky test case tweaks the project configs
 * so it can access a usable Jdk.
 *
 * You must call prepareProject(NAME) at the
 * top of each test and ONLY use the Project
 * instance returned there in the body your
 * tests
 *
 * @author dhleong
 */
abstract class UsableSdkTestCase : BaseTestCase() {

    private var currentProjectName: String = ""
    private var projectFileContents: ByteArray? = null
    private var compilerFileContents: ByteArray? = null
    private val disposable = Disposer.newDisposable()

    override val projectName: String
        get() = currentProjectName

    override fun getProject(): Project {
        throw IllegalStateException("Do not use getProject(); use prepareProject")
    }

    /**
     * Call this ONCE at the top of your test cases to get
     * a Project instance to be used throughout. It's done
     * this way so you can test multiple projects within
     * a single Test file
     *
     * @param projectName
     * @return
     * @throws Exception
     */
    @Throws(Exception::class)
    fun prepareProject(projectName: String): Project {
        currentProjectName = projectName

        // NB get this ONCE for each run (important!)
        //  otherwise, the ModuleRootManager will NOT be the one we
        //  infiltrated with a spy
        val project = ensureProject(getProjectPath(projectName))

        // NB The following stuff modifies stuff on disk. We'll want to restore
        //  all that when we're done
        val module = getModule(project)
        val projectFile = module!!.moduleFile
        val rootDir = projectFile!!.parent
        val compilerFile = rootDir.findChild("compiler.xml")

        projectFileContents = projectFile.contentsToByteArray()
        compilerFileContents = compilerFile!!.contentsToByteArray()

        UIUtil.invokeAndWaitIfNeeded(Runnable {
            JavaTestUtil.setupTestJDK(disposable)
            ModuleRootModificationUtil.setModuleSdk(
                module, JavaTestUtil.getTestJdk())
        })

        CompilerTestUtil.setupJavacForTests(project)
        CompilerTestUtil.enableExternalCompiler()
        CompilerTestUtil.saveApplicationSettings()

        return project
    }

    @Throws(Exception::class)
    override fun tearDown() {
        super.tearDown()

        if (projectFileContents == null || compilerFileContents == null)
            TestCase.fail("Missing project/compiler file contents")

        val project = ensureProject(getProjectPath(currentProjectName!!))

        // NB The following stuff modifies stuff on disk. We'll want to restore
        //  all that when we're done
        val module = getModule(project)
        val projectFile = module!!.moduleFile
        val rootDir = projectFile!!.parent
        val compilerFile = rootDir.findChild("compiler.xml")

        UIUtil.invokeAndWaitIfNeeded(asWriteAction(Runnable {
            try {
                projectFile.setBinaryContent(projectFileContents!!)
                compilerFile!!.setBinaryContent(compilerFileContents!!)
            } catch (e: IOException) {
                e.printStackTrace()
            }
        }))
    }

    companion object {
        protected fun getModule(project: Project): Module? {
            val settings = pickRunSetting(project, null) ?: return null

            val config = settings.configuration
            if (config is ApplicationConfiguration) {
                val configurationModule = config.configurationModule
                return configurationModule.module
            } else if (config is JUnitConfiguration) {
                val config = config
                return config.modules.firstOrNull() // I guess...?
            }

            throw IllegalStateException("Unable to get module for " + project)
        }
    }

}

fun pickRunSetting(project: Project,
                   configuration: String?): RunnerAndConfigurationSettings? {

    val manager = RunManager.getInstance(project)

    // go with "last run"
    if (configuration == null) {
        val selected = manager.selectedConfiguration
        if (selected != null)
            return selected
    }

    for (setting in manager.allSettings) {
        if (configuration == null) {
            return setting // just use the first we see, I guess

        } else if (configuration == setting.name) {
            // found it!
            return setting
        }
    }

    // couldn't find it :(
    return null
}
