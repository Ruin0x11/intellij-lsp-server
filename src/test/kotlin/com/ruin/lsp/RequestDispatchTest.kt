package com.ruin.lsp

import com.fasterxml.jackson.module.kotlin.jacksonObjectMapper
import com.googlecode.jsonrpc4j.ErrorResolver.JsonError
import com.googlecode.jsonrpc4j.JsonRpcBasicServer
import com.nhaarman.mockito_kotlin.mock
import com.nhaarman.mockito_kotlin.verify
import com.ruin.lsp.model.LanguageServerHandler
import com.ruin.lsp.model.createJsonRpcBasicServer
import com.ruin.lsp.values.ClientCapabilities
import org.junit.Assert.assertEquals
import org.junit.Before
import org.junit.Test
import java.io.ByteArrayOutputStream

/**
 * Tests that requests are dispatched to the proper handlers.
 */
class RequestDispatchTest {
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

    @Test
    fun `responds to initialize`() {
        val capabilities = ClientCapabilities(null, null)
        val request = messageWithMapParamsStream("initialize",
                "processId", 1,
                "rootUri", "file:///",
                "capabilities", capabilities
        )
        jsonRpcServer.handleRequest(request, byteArrayOutputStream)
        verify(mockLanguageServerHandler).onInitialize(1, "file:///", capabilities)
    }

    @Test
    fun `responds to initialized`() {
        val request = messageWithMapParamsStream("initialized" )
        jsonRpcServer.handleRequest(request, byteArrayOutputStream)
        verify(mockLanguageServerHandler).onNotifyInitialized()
    }

    @Test
    fun `responds to shutdown`() {
        val request = messageWithMapParamsStream("shutdown" )
        jsonRpcServer.handleRequest(request, byteArrayOutputStream)
        verify(mockLanguageServerHandler).onShutdown()
    }

    @Test
    fun `responds to exit`() {
        val request = messageWithMapParamsStream("exit" )
        jsonRpcServer.handleRequest(request, byteArrayOutputStream)
        verify(mockLanguageServerHandler).onExit()
    }

    @Test
    fun `creates error on invalid method`() {
        val request = messageWithMapParamsStream("<INVALID>" )
        jsonRpcServer.handleRequest(request, byteArrayOutputStream)
        assertEquals(JsonError.METHOD_NOT_FOUND.code, errorCode(getJsonError(byteArrayOutputStream)!!).intValue());
    }
}
