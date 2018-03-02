package com.ruin.lsp.model

import java.net.Socket
import java.io.*

// Not sending two consecutive linefeeds causes the response to have no linefeeds when received makeCompletionParameters an emacs network
// process. I have no idea why.
val SEPARATOR = "\r\r\n"
fun header(name: String, value: Any) = "$name: $value\r\r\n"

class LanguageServerSocket : Socket() {
    private var ins: InputStream? = null
    private var ous: OutputStream? = null

    override fun getInputStream(): InputStream {
        if (ins == null)
            ins = LanguageServerInputStream(super.getInputStream())
        return ins!!
    }

    override fun getOutputStream(): OutputStream {
        if (ous == null) {
            ous = LanguageServerOutputStream(super.getOutputStream())
        }
        return ous!!
    }
}

