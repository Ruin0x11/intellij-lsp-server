package com.ruin.intel.commands.completion

import com.github.kittinunf.result.Result
import com.intellij.openapi.Disposable

interface Command<out T: Any>: Disposable {
    override fun dispose() {

    }

    fun execute(): Result<T, Exception>
}
