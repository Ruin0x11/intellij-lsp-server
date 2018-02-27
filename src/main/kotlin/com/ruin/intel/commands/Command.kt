package com.ruin.intel.commands

import com.github.kittinunf.result.Result

interface Command<out T: Any> {
    fun execute(): Result<T, Exception>
}