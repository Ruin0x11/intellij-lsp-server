package com.ruin.lsp.model

import com.intellij.ide.GeneralSettings
import com.intellij.openapi.application.PreloadingActivity
import com.intellij.openapi.progress.ProgressIndicator
import org.eclipse.lsp4j.jsonrpc.json.StreamMessageProducer
import org.slf4j.LoggerFactory
import java.util.logging.Level
import java.util.logging.Logger

class LanguageServerStartupActivity : PreloadingActivity() {
    private val LOG = LoggerFactory.getLogger(LanguageServerStartupActivity::class.java)

    override fun preload(indicator: ProgressIndicator) {
        LOG.info("Preloading intellij-lsp-server")
        GeneralSettings.getInstance().isShowTipsOnStartup = false
        val server = LanguageServerServiceImpl.getInstance()

        while (true){
            val future = server.connect(SocketConnectionFactory(8080).open())

            // Block until the previous server terminates, then start listening again
            future?.get()
        }
    }
}
