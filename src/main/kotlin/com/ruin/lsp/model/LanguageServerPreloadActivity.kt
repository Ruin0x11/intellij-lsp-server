package com.ruin.lsp.model

import com.intellij.openapi.application.PreloadingActivity
import com.intellij.openapi.progress.ProgressIndicator
import org.slf4j.LoggerFactory

class LanguageServerStartupActivity : PreloadingActivity() {
    private val LOG = LoggerFactory.getLogger(LanguageServerStartupActivity::class.java)

    override fun preload(indicator: ProgressIndicator) {
        LOG.info("Preloading intellij-lsp-server")
        val server = LanguageServerServiceImpl.getInstance()

        val future = server.connect(SocketConnectionFactory(8080).open())
        future?.get()
    }
}
