package com.ruin.lsp.model

import java.io.IOException
import java.net.*


class LanguageServerServerSocket(val port: Int, val backlog: Int, val addr: InetAddress)
    : ServerSocket(port, backlog, addr) {
    @Throws(IOException::class)
    override fun accept(): Socket {
        if (isClosed)
            throw SocketException("Socket is closed")
        if (!isBound)
            throw SocketException("Socket is not bound yet")
        val s = LanguageServerSocket()
        implAccept(s)
        return s
    }
}
