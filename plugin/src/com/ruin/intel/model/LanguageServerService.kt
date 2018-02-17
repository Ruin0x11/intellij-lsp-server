/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package com.ruin.intel.model

import com.intellij.openapi.components.ServiceManager
import com.sun.net.httpserver.HttpHandler
import com.sun.net.httpserver.HttpServer
import java.net.InetSocketAddress
import java.util.concurrent.Executors

open class LanguageServerService {

    private val defaultHostName = "127.0.0.1"

    private var server: HttpServer
    val handler: LanguageServerHttpServerHandler = LanguageServerHttpServerHandler()
    private var isAlive: Boolean = false


    init {
        this.server = createServer(handler)
    }

    private fun createServer(handler: HttpHandler): HttpServer {
        val httpServer = HttpServer.create(InetSocketAddress(defaultHostName, 8080), 5)
        httpServer.executor = Executors.newFixedThreadPool(5)
        httpServer.createContext("/", handler)

        return httpServer
    }

    fun startServer() {
        this.server.start()
        this.isAlive = true
    }

    fun stopServer() {
        this.server.stop(0)
        this.server = createServer(this.handler)
        this.isAlive = false
    }

    fun hasAliveServerProcess(): Boolean = this.isAlive

    companion object {
        fun getInstance() = ServiceManager.getService<LanguageServerService>(LanguageServerService::class.java)!!
    }
}
