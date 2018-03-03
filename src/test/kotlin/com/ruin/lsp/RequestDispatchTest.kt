package com.ruin.lsp

import com.fasterxml.jackson.module.kotlin.jacksonObjectMapper
import com.googlecode.jsonrpc4j.ErrorResolver.JsonError
import com.googlecode.jsonrpc4j.JsonRpcBasicServer
import com.nhaarman.mockito_kotlin.mock
import com.nhaarman.mockito_kotlin.verify
import com.ruin.lsp.model.LanguageServerHandler
import com.ruin.lsp.model.createJsonRpcBasicServer
import com.ruin.lsp.values.ClientCapabilities
import junit.framework.TestCase
import org.junit.Assert.assertEquals
import org.junit.Before
import org.junit.Test
import java.io.ByteArrayOutputStream

/**
 * Tests that requests are dispatched to the proper handlers.
 */
class RequestDispatchTest : TestCase() {
    private val mockLanguageServerHandler: LanguageServerHandler = mock()
    private val jsonRpcServer: JsonRpcBasicServer
    private var byteArrayOutputStream: ByteArrayOutputStream = ByteArrayOutputStream()

    init {
        jsonRpcServer = createJsonRpcBasicServer(jacksonObjectMapper(), mockLanguageServerHandler)
    }

    @Before
    fun setup() {
        byteArrayOutputStream = ByteArrayOutputStream()
    }

    fun `test responds to initialize`() {
        val capabilities = ClientCapabilities(null, null)
        val request = messageWithMapParamsStream("initialize",
                "processId", 1,
                "rootUri", "file:///",
                "capabilities", capabilities
        )
        jsonRpcServer.handleRequest(request, byteArrayOutputStream)
        verify(mockLanguageServerHandler).onInitialize(1, "file:///", capabilities)
    }

    fun `test responds to initialized`() {
        val request = messageWithMapParamsStream("initialized" )
        jsonRpcServer.handleRequest(request, byteArrayOutputStream)
        verify(mockLanguageServerHandler).onNotifyInitialized()
    }

    fun `test responds to shutdown`() {
        val request = messageWithMapParamsStream("shutdown" )
        jsonRpcServer.handleRequest(request, byteArrayOutputStream)
        verify(mockLanguageServerHandler).onShutdown()
    }

    fun `test responds to exit`() {
        val request = messageWithMapParamsStream("exit" )
        jsonRpcServer.handleRequest(request, byteArrayOutputStream)
        verify(mockLanguageServerHandler).onExit()
    }

    fun `test creates error on invalid method`() {
        val request = messageWithMapParamsStream("<INVALID>" )
        jsonRpcServer.handleRequest(request, byteArrayOutputStream)
        assertEquals(JsonError.METHOD_NOT_FOUND.code, errorCode(getJsonError(byteArrayOutputStream)!!).intValue());
    }
}
