package com.ruin.intel.Util

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.project.DumbService
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Computable
import com.intellij.openapi.util.Ref


/**
 * @return A new Computable that wraps execution of the given
 * Computable as a Write Action
 */
fun <T> asWriteAction(computable: Computable<T>): Computable<T> {
    return Computable { ApplicationManager.getApplication().runWriteAction(computable) }
}

/**
 * @return A new Runnable that wraps execution of the given
 * Runnable as a Write Action
 */
fun asWriteAction(runnable: Runnable): Runnable {
    return Runnable { ApplicationManager.getApplication().runWriteAction(runnable) }
}

/**
 * @return A new Runnable that wraps execution of the given
 * Runnable to be "run when smart"
 */
fun whenSmart(project: Project, runWhenSmart: Runnable): Runnable {
    return Runnable {
        val dumbService = DumbService.getInstance(project)
        dumbService.runWhenSmart(runWhenSmart)
    }
}

fun runUndoTransparentAction(runnable: Runnable) {
    CommandProcessor.getInstance().runUndoTransparentAction(runnable)
}

/**
 * Like [UIUtil.invokeAndWaitIfNeeded], but
 * uses [Application.invokeAndWait],
 * so you can safely access PSI stuff
 */
fun <T> invokeAndWaitIfNeeded(computable: Computable<T>): T {
    val ref: Ref<T> = Ref.create()
    val app = ApplicationManager.getApplication()
    app.invokeAndWait({ ref.set(computable.compute()) }, app.defaultModalityState)
    return ref.get()
}