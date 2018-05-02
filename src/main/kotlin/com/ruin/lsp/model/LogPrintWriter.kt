package com.ruin.lsp.model

import com.intellij.openapi.diagnostic.Logger
import org.apache.log4j.Level
import java.io.PrintWriter
import java.io.StringWriter

class LogPrintWriter(private val log: Logger, private val severity: Level, private val writer: StringWriter = StringWriter()) : PrintWriter(writer) {
    override fun flush() {
        val buf = writer.buffer.toString()

        when (severity) {
            Level.WARN -> log.warn(buf)
            Level.ERROR -> log.error(buf)
            Level.DEBUG -> log.debug(buf)
            Level.TRACE -> log.trace(buf)
            else -> log.info(buf)
        }
        super.flush()
    }
}


