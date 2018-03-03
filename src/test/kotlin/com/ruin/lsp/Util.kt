package com.ruin.lsp

import com.fasterxml.jackson.core.JsonProcessingException
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.module.kotlin.jacksonObjectMapper
import com.googlecode.jsonrpc4j.JsonRpcBasicServer
import com.googlecode.jsonrpc4j.JsonRpcServer
import com.ruin.lsp.values.Position
import com.ruin.lsp.values.Range
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.IOException
import java.io.InputStream
import java.nio.charset.StandardCharsets

val JSON_ENCODING = StandardCharsets.UTF_8
val mapper = jacksonObjectMapper()

@Throws(JsonProcessingException::class)
fun messageWithMapParamsStream(methodName: String, vararg args: Any): InputStream {
    return createStream(messageWithMapParams(methodName, args))
}

@Throws(JsonProcessingException::class)
private fun messageWithMapParams(methodName: String, vararg args: Any): HashMap<String, Any> {
    val elements = HashMap<String, Any>()
    val argsArray = args[0] as Array<Any>
    var i = 0
    while (i < argsArray.size) {
        val key = argsArray[i].toString()
        val value = argsArray[i + 1]
        elements.put(key, value)
        i += 2
    }
    return messageOfStream(1, methodName, elements)
}

fun messageOfStream(id: Any, methodName: String, params: Any): HashMap<String, Any> {
    return makeJsonRpcRequestObject(id, methodName, params)
}

private fun makeJsonRpcRequestObject(id: Any?, methodName: String?, params: Any?): HashMap<String, Any> {
    return object : HashMap<String, Any>() {
        init {
            if (id != null) put(JsonRpcBasicServer.ID, id)
            put(JsonRpcBasicServer.JSONRPC, JsonRpcServer.VERSION)
            if (methodName != null) put(JsonRpcBasicServer.METHOD, methodName)
            if (params != null) put(JsonRpcBasicServer.PARAMS, params)
        }
    }
}

@Throws(JsonProcessingException::class)
fun createStream(content: Any): InputStream {
    val data = mapper.writeValueAsString(content)
    return ByteArrayInputStream(data.toByteArray(StandardCharsets.UTF_8))
}

@Throws(IOException::class)
fun decodeAnswer(byteArrayOutputStream: ByteArrayOutputStream): JsonNode {
    return mapper.readTree(byteArrayOutputStream.toString(JSON_ENCODING.name()))
}

@Throws(IOException::class)
fun getJsonError(byteArrayOutputStream: ByteArrayOutputStream): JsonNode? {
    return decodeAnswer(byteArrayOutputStream).get(JsonRpcBasicServer.ERROR)
}

fun errorCode(error: JsonNode): JsonNode {
    return error.get(JsonRpcBasicServer.ERROR_CODE)
}

fun range(startLine: Int, startChar: Int, endLine: Int, endChar: Int): Range =
    Range(Position(startLine, startChar), Position(endLine, endChar))
