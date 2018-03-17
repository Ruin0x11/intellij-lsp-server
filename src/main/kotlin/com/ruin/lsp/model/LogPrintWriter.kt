package com.ruin.lsp.model

import com.intellij.openapi.diagnostic.Logger
import java.io.*

class LogPrintWriter(private val log: Logger, private val writer: StringWriter) : PrintWriter(writer) {
    override fun flush() {
        val buf = writer.buffer.toString()
        log.info(buf)
        super.flush()
    }
}


