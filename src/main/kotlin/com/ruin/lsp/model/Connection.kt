package com.ruin.lsp.model

import java.io.InputStream
import java.io.OutputStream
import java.net.InetAddress
import java.net.ServerSocket

data class Connection(val input: InputStream, val output: OutputStream)

interface ConnectionFactory {
    fun open(): Connection
}

class StdioConnectionFactory : ConnectionFactory {
    override fun open(): Connection = Connection(System.`in`, System.out)
}

class SocketConnectionFactory(private val port: Int) : ConnectionFactory {
    override fun open(): Connection {
        val serverSocket = ServerSocket(port, 50, InetAddress.getByName("127.0.0.1"))
        val clientSocket = serverSocket.accept()
        return Connection(clientSocket.inputStream, clientSocket.outputStream)
    }
}
