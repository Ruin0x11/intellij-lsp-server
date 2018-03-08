package com.ruin.lsp.commands

import com.intellij.openapi.Disposable

interface Command<out T, in S>: Disposable {
    override fun dispose() {}

    fun execute(ctx: S): T
}
