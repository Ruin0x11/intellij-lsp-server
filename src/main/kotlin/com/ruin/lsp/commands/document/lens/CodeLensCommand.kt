package com.ruin.lsp.commands.document.lens

import com.intellij.codeInsight.daemon.LineMarkerInfo
import com.intellij.codeInsight.daemon.impl.LineMarkersPass
import com.intellij.execution.RunnerRegistry
import com.intellij.execution.actions.ConfigurationContext
import com.intellij.execution.actions.ConfigurationFromContext
import com.intellij.execution.configurations.RunConfiguration
import com.intellij.execution.executors.DefaultRunExecutor
import com.intellij.execution.lineMarker.LineMarkerActionWrapper
import com.intellij.execution.runners.ProgramRunner
import com.intellij.icons.AllIcons
import com.intellij.openapi.actionSystem.DefaultActionGroup
import com.intellij.openapi.editor.Document
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.ruin.lsp.commands.DocumentCommand
import com.ruin.lsp.commands.ExecutionContext
import com.ruin.lsp.commands.project.run.description
import com.ruin.lsp.model.RunConfigurationData
import com.ruin.lsp.model.RunConfigurationState
import com.ruin.lsp.util.getDocument
import com.ruin.lsp.util.toRange
import org.eclipse.lsp4j.CodeLens
import org.eclipse.lsp4j.Command
import java.util.*
import javax.swing.Icon

class CodeLensCommand : DocumentCommand<MutableList<CodeLens>> {
    override fun execute(ctx: ExecutionContext): MutableList<CodeLens> {
        val doc = getDocument(ctx.file) ?: return mutableListOf()

        val markers = LineMarkersPass.queryLineMarkers(ctx.file, doc)
        val lineMarkers = markers.mapNotNull { it.codeLens(ctx.file, doc) }

        return lineMarkers.toMutableList()
    }
}

/* copied from RunLineMarkerProvider but modified so actions aren't wrapped (they would become private) */
private fun LineMarkerInfo<PsiElement>.codeLens(file: PsiFile, doc: Document): CodeLens? {
    val actions = (this.createGutterRenderer()?.popupMenuActions as? DefaultActionGroup)?.childActionsOrStubs
        ?.filter { a -> a is LineMarkerActionWrapper }
    if(actions == null || actions.isEmpty()) {
        return null
    }

    // ApplicationRunLineMarkerProvider
    // ExecutorAction wraps RunContextAction

    // By this point we know the run action happens for this element, so we should be able to create a
    // ConfigurationContext from it.
    val configContext = ConfigurationContext(this.element)
    var context = configContext.findExisting()

    if (context == null) {
        // Generate a new config
        val configs = getConfigurationsFromContext(configContext)
        context = configs.firstOrNull()?.configurationSettings ?: return null
    }

    val state = this.createGutterRenderer()?.icon?.runConfigurationState() ?: RunConfigurationState.Run
    val eltRange = this.element?.parent?.textRange?.toRange(doc) ?: return null
    val desc = context.description()

    val data = RunConfigurationData(desc, state)

    return CodeLens().apply {
        this.range = eltRange
        this.command = Command("Run Project", "runProject")
        this.data = data
    }
}

private fun Icon.runConfigurationState() =
    when (this) {
        AllIcons.RunConfigurations.TestState.Run -> RunConfigurationState.Run
        AllIcons.RunConfigurations.TestState.Run_run -> RunConfigurationState.RunClass
        AllIcons.RunConfigurations.TestState.Green2 -> RunConfigurationState.TestPass
        AllIcons.RunConfigurations.TestState.Red2 -> RunConfigurationState.TestFail
        AllIcons.RunConfigurations.TestState.Yellow2 -> RunConfigurationState.TestUnknown
        else -> RunConfigurationState.Run
    }



// items copied from BaseRunConfigurationAction

private fun getConfigurationsFromContext(context: ConfigurationContext): List<ConfigurationFromContext> {
    val fromContext = context.configurationsFromContext ?: return emptyList()

    val enabledConfigurations = ArrayList<ConfigurationFromContext>()
    for (configurationFromContext in fromContext) {
        if (configIsEnabled(configurationFromContext.configuration)) {
            enabledConfigurations.add(configurationFromContext)
        }
    }
    return enabledConfigurations
}

private fun configIsEnabled(configuration: RunConfiguration?): Boolean {
    return getRunner(configuration) != null
}

private fun getRunner(configuration: RunConfiguration?): ProgramRunner<*>? {
    // TODO: The type of runner determines whether or not to run as debug.
    return RunnerRegistry.getInstance().getRunner(DefaultRunExecutor.getRunExecutorInstance().id, configuration)
}
