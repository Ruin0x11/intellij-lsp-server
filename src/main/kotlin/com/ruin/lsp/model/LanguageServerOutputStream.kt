package com.ruin.lsp.model

import java.io.ByteArrayOutputStream
import java.io.FilterOutputStream
import java.io.OutputStream

class LanguageServerOutputStream(stream: OutputStream) : FilterOutputStream(stream) {
    var length: Int = 0
    var ous: ByteArrayOutputStream = ByteArrayOutputStream()

    private fun writeContentLength() {
        out.write(header("Content-Length", length).toByteArray())
        out.write(SEPARATOR.toByteArray())
        out.write(ous.toByteArray())

        length = 0
        ous = ByteArrayOutputStream()
    }

    override fun write(b: Int) {
        // JsonBasicRpcServer.writeAndFlushValue writes a trailing newline after handling a method.
        // use this opportunity to write the Content-Length header to the real output instead.
        // ought to be a better way of doing this.
        if (b == '\n'.toInt()) {
            writeContentLength()
        } else {
            ous.write(b)
        }
    }

    override fun write(b: ByteArray?) {
        length += b!!.size
        ous.write(b)
    }

    override fun write(b: ByteArray?, off: Int, len: Int) {
        length += len
        ous.write(b, off, len)
    }
}
