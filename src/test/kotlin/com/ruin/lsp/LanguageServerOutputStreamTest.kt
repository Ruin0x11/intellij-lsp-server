package com.ruin.lsp

import com.googlecode.jsonrpc4j.JsonRpcBasicServer
import com.googlecode.jsonrpc4j.JsonRpcParam
import com.intellij.testFramework.UsefulTestCase
import com.ruin.lsp.model.LanguageServerOutputStream
import java.io.ByteArrayOutputStream

class LanguageServerOutputStreamTest : UsefulTestCase() {
    fun `test writes Content-Length on newline`() {
        val ins = "dood\n".byteInputStream()
        val bous = ByteArrayOutputStream()
        val ous = LanguageServerOutputStream(bous)
        val buf = ByteArray(4)
        ins.read(buf)
        ous.write(buf)
        ous.write(ins.read())
        val result = String(bous.toByteArray(), Charsets.UTF_8)
        assertEquals("Content-Length: 4\r\r\n\r\r\ndood", result)
    }

    val request = "{\"jsonrpc\":\"2.0\",\"method\":\"handle\",\"params\":{\"thing\":10},\"id\":1}"
    val response = "Content-Length: 36\r\r\n\r\r\n{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":42}"

    fun `test writes Content-Length after handling JSON-RPC`() {
        val handler = TestHandlerImpl()
        val server = JsonRpcBasicServer(handler, TestHandler::class.java)
        val ins = request.byteInputStream()
        val bous = ByteArrayOutputStream()
        val ous = LanguageServerOutputStream(bous)
        server.handleRequest(ins, ous)
        val result = String(bous.toByteArray(), Charsets.UTF_8)
        assertEquals(response, result)
    }
}

abstract class TestHandler {
    abstract fun handle(@JsonRpcParam("thing") thing: Int): Int
}

class TestHandlerImpl : TestHandler() {
    override fun handle(thing: Int): Int {
        return 42
    }
}
