/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package com.ruin.intel.model

import com.googlecode.jsonrpc4j.JsonRpcServer
import com.googlecode.jsonrpc4j.StreamServer
import com.intellij.notification.NotificationDisplayType
import com.intellij.notification.NotificationGroup
import com.intellij.openapi.components.ServiceManager
import com.intellij.openapi.diagnostic.Logger
import com.sun.net.httpserver.HttpServer
import java.net.InetAddress


val IMPORTANT_NOTIFICATIONS = NotificationGroup("LSP Server Notifications", NotificationDisplayType.STICKY_BALLOON, true)

open class LanguageServerService {
    val LOG = Logger.getInstance(LanguageServerService::class.java)

    private val defaultHostName = "127.0.0.1"
    private val defaultPort = 8080

    private var server: LanguageServerStreamingTCPServer = LanguageServerStreamingTCPServer(defaultHostName, defaultPort)
    private var isAlive: Boolean = false

    fun startServer() {
        LOG.info("Start LSP Server!")
        this.server.start()
        this.isAlive = true
    }

    fun stopServer() {
        LOG.info("Stop LSP Server!")
        this.server.stop()
        this.server = LanguageServerStreamingTCPServer(defaultHostName, defaultPort)
        this.isAlive = false
    }

    fun hasAliveServerProcess(): Boolean = this.isAlive
    fun address() = "$defaultHostName:$defaultPort"

    companion object {
        fun getInstance() = ServiceManager.getService<LanguageServerService>(LanguageServerService::class.java)!!
    }
}
