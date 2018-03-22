package com.ruin.lsp.commands.document.completion;

import com.intellij.JavaTestUtil
import com.intellij.execution.application.ApplicationConfiguration
import com.intellij.execution.junit.JUnitConfiguration
import com.intellij.ide.impl.ProjectUtil
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.projectRoots.ProjectJdkTable
import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.UsableSdkTestCase
import com.ruin.lsp.model.invokeCommandAndWait
import com.ruin.lsp.pickRunSetting
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.TextEdit
import kotlin.test.assertNotNull

abstract class CompletionItemResolveCommandTestBase : UsableSdkTestCase() {
     val filePath: String
        get() = DUMMY_FILE_PATH

    override fun setUp() {
        super.setUp()
    }

    override fun initApplication() {
        super.initApplication()
        JavaTestUtil.setupTestJDK(testRootDisposable)
    }

    override fun tearDown() {
        ApplicationManager.getApplication().runWriteAction {
            val table =   ProjectJdkTable.getInstance()

            table.allJdks.forEach { jdk ->
                jdk.sdkModificator.removeAllRoots()
                table.removeJdk(jdk)
            }
        }
        super.tearDown()
    }

    private fun runCommand(pos: Position, selectedItem: String): CompletionItem {
        val completionCommand = CompletionCommand(pos, false)
        val project = prepareProject(JAVA_PROJECT)
        val file = com.ruin.lsp.util.getVirtualFile(project, filePath)
        val completionResult = invokeCommandAndWait(completionCommand, file.url)
        val itemToImport: CompletionItem? = completionResult.right.items.find { it.label == selectedItem }
        assertNotNull(itemToImport, "Item $selectedItem not in completion results: ${completionResult.right.items}")
        val command = CompletionItemResolveCommand(itemToImport!!)
        val res = invokeCommandAndWait(command, file.url)
        ProjectUtil.closeAndDispose(project)
        return res
    }

    protected fun checkHasAdditionalEdits(pos: Position, selectedItem: String, edits: List<TextEdit>) {
        val result = runCommand(pos, selectedItem)

        assertSameElements(edits, result.additionalTextEdits)
    }
}

 fun getModule(project: Project): Module? {
    val settings = pickRunSetting(project, null) ?: return null

    val config = settings.configuration
    if (config is ApplicationConfiguration) {
        val configurationModule = config.configurationModule
        return configurationModule.module
    } else if (config is JUnitConfiguration) {
        return config.modules.firstOrNull() // I guess...?
    }

    throw IllegalStateException("Unable to get module for $project")
}
