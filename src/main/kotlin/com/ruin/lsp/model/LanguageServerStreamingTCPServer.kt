package com.ruin.lsp.model

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.node.ObjectNode
import com.fasterxml.jackson.module.kotlin.jacksonObjectMapper
import com.googlecode.jsonrpc4j.ErrorResolver.JsonError
import com.googlecode.jsonrpc4j.JsonRpcBasicServer
import com.googlecode.jsonrpc4j.StreamServer
import com.intellij.openapi.diagnostic.Logger
import java.net.InetAddress

fun createJsonRpcBasicServer(mapper: ObjectMapper, service: LanguageServerHandler): JsonRpcBasicServer {
    val jsonRpcServer = JsonRpcBasicServer(mapper,
        service, LanguageServerHandler::class.java)
    jsonRpcServer.setErrorResolver(LanguageServerErrorResolver())
    jsonRpcServer.setAllowExtraParams(true)
    jsonRpcServer.setAllowLessParams(true)
    return jsonRpcServer
}

class LanguageServerStreamingTCPServer(hostName: String, port: Int) {
    val LOG = Logger.getInstance(LanguageServerStreamingTCPServer::class.java)

    private val mapper: ObjectMapper = jacksonObjectMapper()
    private var jsonRpcServer: JsonRpcBasicServer
    var context: Context = Context()
    val streamServer: StreamServer

    init {
        val service = LanguageServerHandlerImpl(context)
        this.jsonRpcServer = createJsonRpcBasicServer(mapper, service)

        val maxThreads = 1
        val bindAddress = InetAddress.getByName(hostName)
        val server = LanguageServerServerSocket(port, 50, bindAddress)
        this.streamServer = StreamServer(jsonRpcServer, maxThreads, server)
    }

    fun start() {
        this.streamServer.start()
    }

    fun stop() {
        this.streamServer.stop()
    }
}
