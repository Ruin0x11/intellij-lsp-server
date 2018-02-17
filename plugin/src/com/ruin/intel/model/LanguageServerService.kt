/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package com.ruin.intel.model

import com.googlecode.jsonrpc4j.JsonRpcBasicServer
import com.intellij.openapi.components.ServiceManager
import com.sun.net.httpserver.HttpServer
import java.net.HttpURLConnection
import java.net.InetSocketAddress
import java.util.concurrent.Executors
import java.io.ByteArrayOutputStream




open class LanguageServerService {

    private val defaultHostName = "127.0.0.1"

    private val jsonRpcServer: JsonRpcBasicServer
    private var server: HttpServer
    private var isAlive: Boolean = true

    init {
        val service = LanguageServerHandlerImpl()
        this.jsonRpcServer = JsonRpcBasicServer(service, LanguageServerHandler::class.java)
        this.server = createServer()
        this.server.start()
    }

    private fun createServer(): HttpServer {
        val httpServer = HttpServer.create(InetSocketAddress(defaultHostName, 8080), 5)
        httpServer.executor = Executors.newFixedThreadPool(5)


        httpServer.createContext("/json-rpc") {
            val bos = ByteArrayOutputStream()
            this.jsonRpcServer.handleRequest(it.requestBody, bos)
            it.sendResponseHeaders(HttpURLConnection.HTTP_OK, bos.size().toLong())
            it.responseBody.write(bos.toByteArray())
            it.close()
        }

        return httpServer
    }

    fun startServer() {
        this.server = createServer()
        this.server.start()
        this.isAlive = true
    }

    fun stopServer() {
        this.server.stop(0)
        this.isAlive = false
    }

    fun hasAliveServerProcess(): Boolean = this.isAlive

    companion object {
        fun getInstance() = ServiceManager.getService<LanguageServerService>(LanguageServerService::class.java)!!
    }
}
