/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package com.ruin.lsp.model

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.ServiceManager
import com.intellij.openapi.diagnostic.Logger
import org.apache.log4j.Level
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.launch.LSPLauncher
import java.io.StringWriter
import java.util.concurrent.Future
import java.util.concurrent.atomic.AtomicBoolean

interface LanguageServerService {
    fun connect(connection: Connection): Future<*>?
    fun hasStarted(): Boolean
}

open class LanguageServerServiceImpl : LanguageServerService {
    val LOG = Logger.getInstance(LanguageServerServiceImpl::class.java)

    private var languageServer = MyLanguageServer()
    private val started = AtomicBoolean()
    private var listening: Future<*>? = null

    override fun connect(connection: Connection): Future<*>? {
        LOG.info("Starting the LSP server.")

        ApplicationManager.getApplication().executeOnPooledThread {
            val trace = LogPrintWriter(LOG)
            val launcher = Launcher.createLauncher(languageServer, MyLanguageClient::class.java,
                connection.input, connection.output, false, trace)
            val client = launcher.remoteProxy
            LOG.info("Connecting to client.")
            languageServer.connect(client)
            LOG.info("Listening for commands.")
            listening = launcher.startListening()
        }.get()

        return listening
    }

    override fun hasStarted() = started.get()

    companion object {
        fun getInstance() = ServiceManager.getService<LanguageServerService>(LanguageServerService::class.java)!!
    }
}
