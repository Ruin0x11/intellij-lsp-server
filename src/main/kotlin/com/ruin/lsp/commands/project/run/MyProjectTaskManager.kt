package com.ruin.lsp.commands.project.run

import com.intellij.openapi.compiler.CompileStatusNotification
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Pair
import com.intellij.task.*
import com.intellij.task.impl.AbstractProjectTask
import com.intellij.task.impl.ProjectTaskList
import com.intellij.task.impl.ProjectTaskManagerImpl
import com.intellij.util.Consumer
import com.intellij.util.SmartList
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import java.util.stream.Collectors

/**
 * Massive workaround to get the CompileContext (list of error/warning locations) from a ProjectTask.
 */
class MyProjectTaskManager(val project: Project, val callback: CompileStatusNotification) : ProjectTaskManagerImpl(project) {

    // if only this were protected.
    private val myDefaultProjectTaskRunner = MyProjectTaskRunner(callback)

    override fun run(context: ProjectTaskContext, projectTask: ProjectTask, callback: ProjectTaskNotification?) {
        val toRun = SmartList<Pair<ProjectTaskRunner, Collection<ProjectTask>>>()

        val taskClassifier = Consumer { tasks: Collection<ProjectTask>->
            val toBuild = tasks.stream().collect(Collectors.groupingBy<ProjectTask, ProjectTaskRunner> { aTask ->
                for (runner in getTaskRunners()) {
                    // TODO: find a way to send CompileContext with custom runners
                    if (runner.canRun(aTask)) return@groupingBy runner
                }
                myDefaultProjectTaskRunner
            })
            for (entry in toBuild.entries) {
                toRun.add(Pair.create(entry.key, entry.value))
            }
        }
        visitTasks(projectTask as? ProjectTaskList ?: setOf(projectTask), taskClassifier)

        if (toRun.isEmpty()) {
            sendSuccessNotify(callback)
            return
        }

        val inProgressCounter = AtomicInteger(toRun.size)
        val errorsCounter = AtomicInteger()
        val warningsCounter = AtomicInteger()
        val abortedFlag = AtomicBoolean(false)
        val chunkStatusNotification = if (callback == null)
            null
        else
            ProjectTaskNotification { executionResult ->
                val inProgress = inProgressCounter.decrementAndGet()
                val allErrors = errorsCounter.addAndGet(executionResult.errors)
                val allWarnings = warningsCounter.addAndGet(executionResult.warnings)
                if (executionResult.isAborted) {
                    abortedFlag.set(true)
                }
                if (inProgress == 0) {
                    callback.finished(ProjectTaskResult(abortedFlag.get(), allErrors, allWarnings))
                }
            }

        toRun.forEach { pair ->
            if (pair.second.isEmpty()) {
                sendSuccessNotify(chunkStatusNotification)
            } else {
                pair.first.run(myProject, context, chunkStatusNotification, pair.second)
            }
        }
    }

    private fun visitTasks(tasks: Collection<ProjectTask>,
                           consumer: Consumer<Collection<ProjectTask>>) {
        for (child in tasks) {
            val taskDependencies: Collection<ProjectTask> = if (child is AbstractProjectTask) {
                child.dependsOn
            } else child as? ProjectTaskList ?: setOf(child)

            visitTasks(taskDependencies, consumer)
        }
        consumer.consume(tasks)
    }

    private fun sendSuccessNotify(notification: ProjectTaskNotification?) {
        notification?.finished(ProjectTaskResult(false, 0, 0))
    }

    private fun getTaskRunners(): Array<ProjectTaskRunner> {
        return ProjectTaskRunner.EP_NAME.extensions
    }
}
