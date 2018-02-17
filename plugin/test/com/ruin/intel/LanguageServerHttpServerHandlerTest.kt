package test.com.ruin.intel

import com.googlecode.jsonrpc4j.ErrorResolver
import com.nhaarman.mockito_kotlin.mock
import com.ruin.intel.errorCode
import com.ruin.intel.getJsonError
import com.ruin.intel.messageWithMapParamsStream
import com.ruin.intel.model.LanguageServerHttpServerHandler
import com.sun.net.httpserver.HttpExchange
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Test
import org.mockito.BDDMockito.given
import java.io.ByteArrayOutputStream

class LanguageServerHttpServerHandlerTest {

    private val handler: LanguageServerHttpServerHandler = LanguageServerHttpServerHandler()
    private val mockExchange: HttpExchange = mock()
    var response: ByteArrayOutputStream = ByteArrayOutputStream()

    fun sendCommand(command: String) {
        response = ByteArrayOutputStream()
        given(mockExchange.requestBody).willReturn(messageWithMapParamsStream(command))
        given(mockExchange.responseBody).willReturn(response)
        handler.handle(mockExchange)
    }

    @Test
    fun `drops commands before initialize`() {
        sendCommand("shutdown")
        val error = getJsonError(response)
        assertNotNull(error)
        assertEquals(ErrorResolver.JsonError.BULK_ERROR.code, errorCode(error!!).intValue())
    }

    @Test
    fun `allows commands after initialize`() {
        sendCommand("initialize")
        sendCommand("shutdown")
    }
}