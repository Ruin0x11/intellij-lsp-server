package com.ruin.lsp.commands

import com.intellij.openapi.Disposable
import com.intellij.openapi.project.Project

/**
 * A command that is run on an entire project.
 */
interface ProjectCommand<out T: Any?>: Command<T, Project>
