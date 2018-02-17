package com.ruin.intel.model

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.node.ObjectNode
import com.fasterxml.jackson.module.kotlin.jacksonObjectMapper
import com.googlecode.jsonrpc4j.ErrorResolver.JsonError
import com.googlecode.jsonrpc4j.JsonRpcBasicServer
import com.sun.net.httpserver.HttpExchange
import com.sun.net.httpserver.HttpHandler
import com.sun.xml.internal.ws.util.NoCloseOutputStream
import java.io.ByteArrayOutputStream
import java.io.IOException
import java.io.OutputStream
import java.math.BigDecimal
import java.net.HttpURLConnection


class LanguageServerHttpServerHandler: HttpHandler {

    private val mapper: ObjectMapper = jacksonObjectMapper()
    private var jsonRpcServer: JsonRpcBasicServer
    var wasInitialized: Boolean = false

    init {
        val service = LanguageServerHandlerImpl(this)
        this.jsonRpcServer = JsonRpcBasicServer(mapper,
                service, LanguageServerHandler::class.java)
        this.jsonRpcServer.setErrorResolver(LanguageServerErrorResolver())
    }

    override fun handle(exchange: HttpExchange?) {
        val bos = ByteArrayOutputStream()
        this.jsonRpcServer.handleRequest(exchange?.requestBody, bos)
        exchange?.sendResponseHeaders(HttpURLConnection.HTTP_OK, bos.size().toLong())
        exchange?.responseBody?.write(bos.toByteArray())

        exchange?.responseBody?.close()
    }

    // NOTE: duplication from jsonrpc4j
    private fun createResponseError(jsonRpc: String, id: Any, errorObject: JsonError): ObjectNode {
        val response = mapper.createObjectNode()
        val error = mapper.createObjectNode()
        error.put(JsonRpcBasicServer.ERROR_CODE, errorObject.code)
        error.put(JsonRpcBasicServer.ERROR_MESSAGE, errorObject.message)
        if (errorObject.data != null) {
            error.set(JsonRpcBasicServer.DATA, mapper.valueToTree(errorObject.data))
        }
        response.put(JsonRpcBasicServer.JSONRPC, jsonRpc)
        if (Int::class.java.isInstance(id)) {
            response.put(JsonRpcBasicServer.ID, Int::class.java.cast(id).toInt())
        } else if (Long::class.java.isInstance(id)) {
            response.put(JsonRpcBasicServer.ID, Long::class.java.cast(id).toLong())
        } else if (Float::class.java.isInstance(id)) {
            response.put(JsonRpcBasicServer.ID, Float::class.java.cast(id).toFloat())
        } else if (Double::class.java.isInstance(id)) {
            response.put(JsonRpcBasicServer.ID, Double::class.java.cast(id).toDouble())
        } else if (BigDecimal::class.java.isInstance(id)) {
            response.put(JsonRpcBasicServer.ID, BigDecimal::class.java.cast(id))
        } else {
            response.put(JsonRpcBasicServer.ID, String::class.java.cast(id))
        }
        response.set(JsonRpcBasicServer.ERROR, error)
        return response
    }

    @Throws(IOException::class)
    private fun writeAndFlushValue(output: OutputStream, value: ObjectNode) {
        mapper.writeValue(NoCloseOutputStream(output), value)
        output.write("\n".toByteArray())
    }
}