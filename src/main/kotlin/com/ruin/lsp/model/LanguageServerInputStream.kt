package com.ruin.lsp.model

import sun.rmi.transport.proxy.RMIMasterSocketFactory
import java.io.*
import java.io.IOException
import javax.xml.ws.http.HTTPException
import java.io.ByteArrayInputStream
import java.io.InputStreamReader
import java.io.BufferedReader

/**
 * An input stream that strips out the leading Content-Length header that the LSP protocol adds.
 */
class LanguageServerInputStream(stream: InputStream) : FilterInputStream(stream) {

    protected var bytesLeft: Int = 0
    protected var bytesLeftAtMark: Int = 0

    init {
        if (`in`.markSupported())
            `in`.mark(0) // prevent resetting back to old marks

        bytesLeft = readContentLength()
        bytesLeftAtMark = bytesLeft
    }

    private fun readContentLength(): Int {
        // we can't use a BufferedReader directly on the input stream, since it will buffer past the end of the headers
        var buf = ByteArray(4096)
        var buflen = 0
        while (true) {
            val read = `in`.read()
            if (read == -1 && buflen == 0) return 0
            if (read == -1) return 0
            buf[buflen++] = read.toByte()
            if (buflen >= 4 && buf[buflen - 4] == '\r'.toByte() && buf[buflen - 3] == '\n'.toByte() &&
                buf[buflen - 2] == '\r'.toByte() && buf[buflen - 1] == '\n'.toByte())
                break
            if (buflen >= 2 && buf[buflen - 1] == '\n'.toByte() && buf[buflen - 2] == '\n'.toByte())
                break  // nice for people using stdio
            if (buflen == buf.size) {
                val newbuf = ByteArray(buf.size * 2)
                System.arraycopy(buf, 0, newbuf, 0, buflen)
                buf = newbuf
            }
        }

        val br = BufferedReader(InputStreamReader(ByteArrayInputStream(buf, 0, buflen)))
        val s: String = br.readLine()
        val front = s.substring(0, s.indexOf(':')).toLowerCase()
        val back = s.substring(s.indexOf(':') + 1).trim { it <= ' ' }
        if (front != "Content-Length".toLowerCase()) {
            return 0
        }
        return back.toInt()
    }

    @Throws(IOException::class)
    override fun available(): Int {
        var bytesAvailable = `in`.available()
        if (bytesAvailable > bytesLeft)
            bytesAvailable = bytesLeft

        return bytesAvailable
    }

    @Throws(IOException::class)
    override fun read(): Int {
        while (bytesLeft == 0) {
            bytesLeft = readContentLength()
        }
        val data = `in`.read()
        --bytesLeft
        return data
    }

    @Throws(IOException::class)
    override fun read(b: ByteArray, off: Int, len: Int): Int {
        var toRead = len
        if (toRead > 0) {
            while (bytesLeft == 0) {
                bytesLeft = readContentLength()
            }
        }
        if (toRead > bytesLeft)
            toRead = bytesLeft
        val bytesRead = `in`.read(b, off, toRead)
        bytesLeft -= bytesRead

        return bytesRead
    }

    override fun mark(readlimit: Int) {
        `in`.mark(readlimit)
        if (`in`.markSupported())
            bytesLeftAtMark = bytesLeft
    }

    @Throws(IOException::class)
    override fun reset() {
        `in`.reset()
        bytesLeft = bytesLeftAtMark
    }

    @Throws(IOException::class)
    override fun skip(n: Long): Long {
        var count = n
        if (count > bytesLeft)
            count = bytesLeft.toLong()
        val bytesSkipped = `in`.skip(count)
        bytesLeft -= bytesSkipped.toInt()
        return bytesSkipped
    }
}
