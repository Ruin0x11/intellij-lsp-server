package com.ruin.lsp.commands.project.run

import com.intellij.execution.configurations.RunConfiguration
import com.intellij.execution.impl.ExecutionManagerImpl
import com.intellij.openapi.compiler.CompileScope
import com.intellij.openapi.compiler.CompileStatusNotification
import com.intellij.openapi.compiler.CompilerManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.text.StringUtil
import com.intellij.packaging.artifacts.Artifact
import com.intellij.packaging.impl.compiler.ArtifactCompileScope
import com.intellij.packaging.impl.compiler.ArtifactsWorkspaceSettings
import com.intellij.task.*
import com.intellij.util.SmartList
import com.intellij.util.containers.ContainerUtil
import java.util.stream.Collectors

/**
 * Literally the same as InternalProjectTaskRunner, except allows running a callback with the CompileContext as an
 * argument.
 *
 * This allows sending the actual error locations back to the language client.
 */
class MyProjectTaskRunner(private val trueCallback: CompileStatusNotification) : ProjectTaskRunner() {

    override fun run(project: Project,
                     context: ProjectTaskContext,
                     callback: ProjectTaskNotification?,
                     tasks: Collection<ProjectTask>) {
        val compileNotification = if (callback == null)
            null
        else
            CompileStatusNotification { aborted, errors, warnings, compileContext ->
                trueCallback.finished(aborted, errors, warnings, compileContext)
                callback.finished(ProjectTaskResult(aborted, errors, warnings))
            }

        val taskMap = groupBy(tasks)
        runModulesBuildTasks(project, context, compileNotification, taskMap)
        runFilesBuildTasks(project, compileNotification, taskMap)
        runArtifactsBuildTasks(project, context, compileNotification, taskMap)
    }

    override fun canRun(projectTask: ProjectTask): Boolean {
        return true
    }

    private class ModulesBuildSettings(internal val isIncrementalBuild: Boolean,
                                       internal val includeDependentModules: Boolean,
                                       internal val includeRuntimeDependencies: Boolean,
                                       internal val modules: Collection<Module>)

    companion object {
        private val LOG = Logger.getInstance(MyProjectTaskRunner::class.java)
        val EXECUTION_SESSION_ID_KEY = ExecutionManagerImpl.EXECUTION_SESSION_ID_KEY

        fun groupBy(tasks: Collection<ProjectTask>): Map<Class<out ProjectTask>, List<ProjectTask>> {
            return tasks.stream().collect(Collectors.groupingBy({ o ->
                if (o is ModuleFilesBuildTask) return@groupingBy ModuleFilesBuildTask::class.java
                if (o is ModuleBuildTask) return@groupingBy ModuleBuildTask::class.java
                if (o is ArtifactBuildTask) return@groupingBy ArtifactBuildTask::class.java
                o.javaClass
            }))
        }

        private fun runModulesBuildTasks(project: Project,
                                         context: ProjectTaskContext,
                                         compileNotification: CompileStatusNotification?,
                                         tasksMap: Map<Class<out ProjectTask>, List<ProjectTask>>) {
            val buildTasks = tasksMap[ModuleBuildTask::class.java]
            if (ContainerUtil.isEmpty(buildTasks)) return
            val modulesBuildSettings = assembleModulesBuildSettings(buildTasks!!)

            val compilerManager = CompilerManager.getInstance(project)
            val scope = createScope(compilerManager, context,
                modulesBuildSettings.modules,
                modulesBuildSettings.includeDependentModules,
                modulesBuildSettings.includeRuntimeDependencies)
            if (modulesBuildSettings.isIncrementalBuild) {
                compilerManager.make(scope, compileNotification)
            } else {
                compilerManager.compile(scope, compileNotification)
            }
        }

        private fun assembleModulesBuildSettings(buildTasks: Collection<ProjectTask>): ModulesBuildSettings {
            val modules = SmartList<Module>()
            val incrementalTasks = ContainerUtil.newSmartList<ModuleBuildTask>()
            val excludeDependentTasks = ContainerUtil.newSmartList<ModuleBuildTask>()
            val excludeRuntimeTasks = ContainerUtil.newSmartList<ModuleBuildTask>()

            for (buildProjectTask in buildTasks) {
                val moduleBuildTask = buildProjectTask as ModuleBuildTask
                modules.add(moduleBuildTask.module)

                if (moduleBuildTask.isIncrementalBuild) {
                    incrementalTasks.add(moduleBuildTask)
                }
                if (!moduleBuildTask.isIncludeDependentModules) {
                    excludeDependentTasks.add(moduleBuildTask)
                }
                if (!moduleBuildTask.isIncludeRuntimeDependencies) {
                    excludeRuntimeTasks.add(moduleBuildTask)
                }
            }

            val isIncrementalBuild = incrementalTasks.size == buildTasks.size
            val includeDependentModules = excludeDependentTasks.size != buildTasks.size
            val includeRuntimeDependencies = excludeRuntimeTasks.size != buildTasks.size

            if (!isIncrementalBuild && !incrementalTasks.isEmpty()) {
                assertModuleBuildSettingsConsistent(incrementalTasks, "will be built ignoring incremental build setting")
            }
            if (includeDependentModules && !excludeDependentTasks.isEmpty()) {
                assertModuleBuildSettingsConsistent(excludeDependentTasks, "will be built along with dependent modules")
            }
            if (includeRuntimeDependencies && !excludeRuntimeTasks.isEmpty()) {
                assertModuleBuildSettingsConsistent(excludeRuntimeTasks, "will be built along with runtime dependencies")
            }
            return ModulesBuildSettings(isIncrementalBuild, includeDependentModules, includeRuntimeDependencies, modules)
        }

        private fun assertModuleBuildSettingsConsistent(moduleBuildTasks: Collection<ModuleBuildTask>, warnMsg: String) {
            val moduleNames = StringUtil.join(moduleBuildTasks, { task -> task.module.name }, ", ")
            LOG.warn("Module" + (if (moduleBuildTasks.size > 1) "s" else "") + " : '" + moduleNames + "' " + warnMsg)
        }

        private fun createScope(compilerManager: CompilerManager,
                                context: ProjectTaskContext,
                                modules: Collection<Module>,
                                includeDependentModules: Boolean,
                                includeRuntimeDependencies: Boolean): CompileScope {
            val scope = compilerManager.createModulesCompileScope(
                modules.toTypedArray(), includeDependentModules, includeRuntimeDependencies)
            val configuration = context.runConfiguration
            if (configuration != null) {
                scope.putUserData(CompilerManager.RUN_CONFIGURATION_KEY, configuration)
                scope.putUserData(CompilerManager.RUN_CONFIGURATION_TYPE_ID_KEY, configuration.type.id)
            }
            EXECUTION_SESSION_ID_KEY.set(scope, context.sessionId)
            return scope
        }

        private fun runFilesBuildTasks(project: Project,
                                       compileNotification: CompileStatusNotification?,
                                       tasksMap: Map<Class<out ProjectTask>, List<ProjectTask>>) {
            val filesTargets = tasksMap[ModuleFilesBuildTask::class.java]
            if (!ContainerUtil.isEmpty(filesTargets)) {
                val files = filesTargets!!
                    .flatMap { target -> (target as ModuleFilesBuildTask).files.toList() }.toTypedArray()
                CompilerManager.getInstance(project).compile(files, compileNotification)
            }
        }

        private fun runArtifactsBuildTasks(project: Project,
                                           context: ProjectTaskContext,
                                           compileNotification: CompileStatusNotification?,
                                           tasksMap: Map<Class<out ProjectTask>, List<ProjectTask>>) {

            val buildTasks = tasksMap[ArtifactBuildTask::class.java]
            if (!ContainerUtil.isEmpty(buildTasks)) {
                val toMake = SmartList<Artifact>()
                val toCompile = SmartList<Artifact>()
                for (buildProjectTask in buildTasks!!) {
                    val artifactBuildTask = buildProjectTask as ArtifactBuildTask

                    if (artifactBuildTask.isIncrementalBuild) {
                        toMake.add(artifactBuildTask.artifact)
                    } else {
                        toCompile.add(artifactBuildTask.artifact)
                    }
                }

                buildArtifacts(project, toMake, context.sessionId, compileNotification, false)
                buildArtifacts(project, toCompile, context.sessionId, compileNotification, true)
            }
        }

        private fun buildArtifacts(project: Project,
                                   artifacts: List<Artifact>,
                                   sessionId: Any?,
                                   compileNotification: CompileStatusNotification?,
                                   forceArtifactBuild: Boolean) {
            if (!artifacts.isEmpty()) {
                val scope = ArtifactCompileScope.createArtifactsScope(project, artifacts, forceArtifactBuild)
                ArtifactsWorkspaceSettings.getInstance(project).setArtifactsToBuild(artifacts)
                EXECUTION_SESSION_ID_KEY.set(scope, sessionId)
                //in external build we can set 'rebuild' flag per target type
                CompilerManager.getInstance(project).make(scope, compileNotification)
            }
        }
    }
}
