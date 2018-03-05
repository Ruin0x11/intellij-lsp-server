package com.ruin.lsp.model

import com.intellij.openapi.application.ApplicationStarterEx

class LspApplicationStarter : ApplicationStarterEx() {
    private val defaultPort = 8080

    override fun getCommandName(): String = "lang-server"
    override fun isHeadless(): Boolean = true
    override fun canProcessExternalCommandLine(): Boolean = true

    override fun processExternalCommandLine(args: Array<out String>, currentDirectory: String?) {
        super.processExternalCommandLine(args, currentDirectory)
    }

    override fun premain(args: Array<out String>) {
        // pass
    }

    override fun main(a: Array<out String>) {
        val server = LanguageServerServiceImpl.getInstance()

        val future = server.connect(SocketConnectionFactory(defaultPort).open())
        future?.get()
    }
}
