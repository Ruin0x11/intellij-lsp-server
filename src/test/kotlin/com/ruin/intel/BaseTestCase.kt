package com.ruin.intel

import com.intellij.ide.impl.ProjectUtil
import com.intellij.openapi.project.Project
import com.intellij.testFramework.fixtures.LightPlatformCodeInsightFixtureTestCase
import com.ruin.intel.Util.ensureProject
import java.io.File

val JAVA_PROJECT = "java-project"
val LOOPING_PROJECT = "looping-project"
val RUNNABLE_PROJECT = "runnable-project"
val TESTABLE_PROJECT = "testable-project"
val DUMMY_FILE_PATH = "src/org/intellivim/javaproject/Dummy.java"
val SUBCLASS_FILE_PATH = "src/org/intellivim/javaproject/SubClass.java"
val INTERFACE_FILE_PATH = "src/org/intellivim/javaproject/MyInterface.java"
val PROBLEMATIC_FILE_PATH = "src/org/intellivim/javaproject/Problematic.java"
val PROBLEMATIC_TWICE_FILE_PATH = "src/org/intellivim/javaproject/ProblematicTwice.java"

abstract class BaseTestCase : LightPlatformCodeInsightFixtureTestCase() {

    /** Which example project does this test reference?  */
    protected abstract val projectName: String

    protected fun getProjectIml(projectName: String): String {
        val root = File(".")
        val pathToIml = "projects/$projectName/$projectName.iml"
        return File(root, pathToIml).canonicalFile.absolutePath
    }

    protected fun getProjectPath(projectName: String): String {
        val root = File(".")
        val pathToProject = "projects/$projectName/"
        return File(root, pathToProject).canonicalFile.absolutePath
    }

    protected fun getProjectPath() = getProjectPath(projectName)
    protected fun getProjectIml() = getProjectIml(projectName)

    override fun tearDown() {
        ProjectUtil.closeAndDispose(getProject())
        super.tearDown()
    }

    override fun getProject(): Project {
        val path = getProjectIml(projectName)
        return ensureProject(path)
    }

    protected fun fixtureProject() = myFixture.project
}
