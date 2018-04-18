package com.ruin.lsp

import com.intellij.ide.impl.ProjectUtil
import com.intellij.openapi.project.Project
import com.intellij.testFramework.fixtures.LightPlatformCodeInsightFixtureTestCase
import com.ruin.lsp.util.ensureProject
import com.ruin.lsp.util.getURIForFile
import com.ruin.lsp.values.DocumentUri
import org.jetbrains.kotlin.utils.fileUtils.withReplacedExtensionOrNull
import java.io.File

val JAVA_PROJECT = "java-project"
val KOTLIN_PROJECT = "kotlin-project"
val LOOPING_PROJECT = "looping-project"
val RUNNABLE_PROJECT = "runnable-project"
val TESTABLE_PROJECT = "testable-project"
val MULTI_MODULE_PROJECT = "multi-module-project"
val MAVEN_MULTI_MODULE_PROJECT = "maven-multi-module-project"
val DUMMY_FILE_PATH = "src/org/lsp/javaproject/Dummy.java"
val SUBCLASS_FILE_PATH = "src/org/lsp/javaproject/SubClass.java"
val INTERFACE_FILE_PATH = "src/org/lsp/javaproject/MyInterface.java"
val DATA_FILE_PATH = "src/org/lsp/javaproject/MyData.java"
val TESTABLE_FILE_PATH = "src/org/lsp/runnable/test/Testable.java"
val PROBLEMATIC_FILE_PATH = "src/org/lsp/javaproject/Problematic.java"
val PROBLEMATIC_TWICE_FILE_PATH = "src/org/lsp/javaproject/ProblematicTwice.java"
val CONSTANTS_FILE_PATH = "src/org/lsp/javaproject/Constants.java"
val ENUM_TYPE_FILE_PATH = "src/org/lsp/javaproject/EnumType.java"

val OBJECT_FILE_PATH = "src/org/lsp/kotlinproject/MyObject.kt"

val MULTI_MODULE_APP_PATH = "application/src/main/java/hello/app/DemoApplication.java"
val MAVEN_MULTI_MODULE_APP_PATH = "first-module/src/main/java/com/ruin/lsp/App.java"

abstract class BaseTestCase : LightPlatformCodeInsightFixtureTestCase() {

    /** Which example project does this test reference?  */
    protected abstract val projectName: String


    protected fun getProjectPath() = getProjectPath(projectName)
    protected fun getProjectIml() = getProjectIml(projectName)

    override fun getProject(): Project {
        val path = getProjectPath(projectName)
        return ensureProject(path)
    }

    protected fun fixtureProject() = myFixture.project

    override fun tearDown() {
        ProjectUtil.closeAndDispose(project)
        super.tearDown()
    }
}

fun getProjectIml(projectName: String): String {
    val root = File(".")
    val pathToIml = "projects/$projectName/$projectName.iml"
    return File(root, pathToIml).canonicalFile.absolutePath
}

fun getProjectPath(projectName: String): String {
    val root = File(".")
    val pathToProject = "projects/$projectName/"
    return File(root, pathToProject).canonicalFile.absolutePath
}

fun uriForPath(projectName: String, filePath: String): DocumentUri {
    val projectPath = getProjectPath(projectName)
    val file = File(projectPath, filePath)
    return getURIForFile(file)
}

fun forKotlin(path: String) =
    File(path.replace("javaproject", "kotlinproject"))
    .withReplacedExtensionOrNull("java", "kt")!!.path
