package com.ruin.lsp.model

import com.intellij.openapi.diagnostic.Logger
import java.io.PrintWriter
import java.io.StringWriter

class LogPrintWriter(private val log: Logger, private val writer: StringWriter = StringWriter()) : PrintWriter(writer) {
    override fun flush() {
        val buf = writer.buffer.toString()
        log.debug(buf)
        super.flush()
    }
}


