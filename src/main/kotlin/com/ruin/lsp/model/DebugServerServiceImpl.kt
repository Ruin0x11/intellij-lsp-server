package com.ruin.lsp.model

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.ServiceManager
import com.intellij.openapi.diagnostic.Logger
import org.eclipse.lsp4j.debug.*
import org.eclipse.lsp4j.debug.launch.DSPLauncher
import org.eclipse.lsp4j.debug.services.IDebugProtocolClient
import org.eclipse.lsp4j.debug.services.IDebugProtocolServer
import org.eclipse.lsp4j.jsonrpc.Launcher
import java.util.concurrent.CompletableFuture
import java.util.concurrent.Future
import java.util.concurrent.atomic.AtomicBoolean

interface DebugServerService {
    fun connect(connection: Connection): Future<*>?
    fun hasStarted(): Boolean
}

open class DebugServerServiceImpl : DebugServerService {
    val LOG = Logger.getInstance(DebugServerServiceImpl::class.java)

    private var debugServer = MyDebugServer()
    private val started = AtomicBoolean()
    private var listening: Future<*>? = null

    override fun connect(connection: Connection): Future<*>? {
        if (!started.compareAndSet(false, true)) {
            LOG.warn("Server was already started.")
            return null
        }

        LOG.info("Starting the debug server.")

        return ApplicationManager.getApplication().executeOnPooledThread {
            val trace = LogPrintWriter(LOG)
            val launcher = DSPLauncher.createServerLauncher(debugServer,
                connection.input, connection.output, false, trace)
            // TODO handle other connection types
            LOG.info("Listening for commands.")
            listening = launcher.startListening()
        }
    }

    override fun hasStarted() = started.get()

    companion object {
        fun getInstance() = ServiceManager.getService<DebugServerService>(DebugServerService::class.java)!!
    }
}
