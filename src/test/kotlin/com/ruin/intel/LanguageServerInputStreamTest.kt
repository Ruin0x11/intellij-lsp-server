package com.ruin.intel;

import com.intellij.testFramework.UsefulTestCase
import com.ruin.intel.model.LanguageServerInputStream
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

class LanguageServerInputStreamTest : UsefulTestCase() {
    val request ="Content-Length: 475\r\n\r\n" +
        "{\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"params\":{\"processId\":5100," +
        "\"rootPath\":\"e:/build/intellij-plugins/Dart/\"," +
        "\"rootUri\":\"file:///e:/build/intellij-plugins/Dart/\"," +
        "\"capabilities\":{\"workspace\":{\"applyEdit\":true,\"executeCommand\":{\"dynamicRegistration\":true}}," +
        "\"textDocument\":{\"synchronization\":{\"willSave\":true,\"didSave\":true}," +
        "\"documentSymbol\":{\"symbolKind\":{\"valueSet\":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]}}}}," +
        "\"initializationOptions\":null},\"id\":1}"

    fun `test reads protocol data`() {
        val br = LanguageServerInputStream(request.byteInputStream(StandardCharsets.UTF_8))
        assertEquals(475, br.available())
        val buf = ByteArray(475)
        br.read(buf)
        assertEquals(0, br.available())
    }

    fun `test stays open after one message`() {
        val twoRequests = request + request
        val br = LanguageServerInputStream(twoRequests.byteInputStream(StandardCharsets.UTF_8))
        val buf = ByteArray(475)
        br.read(buf)

        // Read the header, then one byte
        br.read()
        assertEquals(474, br.available())
    }

    val lengthZeroRequest = "Content-Length: 0\r\n\r\n"

    fun `test reads multiple length zero messages`() {
        val fiveRequests = request + lengthZeroRequest.repeat(3)+ request
        val br = LanguageServerInputStream(fiveRequests.byteInputStream(StandardCharsets.UTF_8))
        val buf = ByteArray(475)
        br.read(buf)

        // Read past the three empty requests, the header of the filled one, then one byte
        br.read()
        assertEquals(474, br.available())
    }
}
