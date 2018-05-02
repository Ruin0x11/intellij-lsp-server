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
import java.io.IOException
import java.net.InetAddress
import java.net.ServerSocket
import java.net.Socket
import java.util.concurrent.Future
import java.util.concurrent.SynchronousQueue
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

interface LanguageServerRunner {
    fun run(port: Int)
}

private val LOG = Logger.getInstance(LanguageServerRunner::class.java)

class LanguageServerRunnerImpl : LanguageServerRunner {
    private var serverSocket: ServerSocket? = null
    private val executor = ThreadPoolExecutor(0, 100, 30L, TimeUnit.SECONDS, SynchronousQueue<Runnable>())
    private var isStarted = false

    override fun run(port: Int) {
        if (!isStarted) {
            LOG.info("Starting the LSP server on port $port.")
            serverSocket = ServerSocket(port, 50, InetAddress.getByName("127.0.0.1"))
            if (serverSocket != null) {
                isStarted = true
                ApplicationManager.getApplication().executeOnPooledThread {
                    while (true) {
                        try {
                            val clientSocket = serverSocket!!.accept()
                            executor.submit(createConnectionTask(clientSocket))
                        } catch (e: IOException) {
                            LOG.error("Socket connection error: $e")
                            closeServerSocket()
                            shutdownConnectionPool(false)
                            return@executeOnPooledThread
                        }
                    }
                }
            }
        }
    }

    @Synchronized
    private fun closeServerSocket() {
        if (serverSocket != null) {
            try {
                LOG.info("Close language server socket port " + serverSocket!!.localPort)
                serverSocket!!.close()
            } catch (e: IOException) {
                LOG.error("Close ServerSocket exception: $e")
            }

        }
        serverSocket = null
    }


    @Synchronized
    private fun shutdownConnectionPool(now: Boolean) {
        if (now) {
            this.executor.shutdownNow()
        } else {
            this.executor.shutdown()
        }
    }

    private fun createConnectionTask(connection: Socket) =
        Runnable {
            try {
                val server = LanguageServerService()
                server.connect(connection)
            } catch(e: IOException) {
                LOG.error("Client socket connection error: $e")
            } finally {
                LOG.info("LSP connection closed")
            }
        }

    companion object {
        @Synchronized
        fun getInstance() = ServiceManager.getService<LanguageServerRunner>(LanguageServerRunner::class.java)!!
    }
}

open class LanguageServerService {
    private var languageServer = MyLanguageServer()
    private val started = AtomicBoolean()

    fun connect(connection: Socket): Future<*>? {
        if (!started.compareAndSet(false, true)) {
            LOG.warn("Server was already started.")
            return null
        }

        return ApplicationManager.getApplication().executeOnPooledThread {
            val trace = LogPrintWriter(LOG, Level.DEBUG)
            val launcher = Launcher.createLauncher(languageServer, MyLanguageClient::class.java,
                connection.getInputStream(), connection.getOutputStream(), false, trace)
            val client = launcher.remoteProxy
            // TODO handle other connection types
            LOG.info("Connecting to client.")
            languageServer.connect(client)
            LOG.info("Listening for commands.")
            launcher.startListening()
        }
    }
}
