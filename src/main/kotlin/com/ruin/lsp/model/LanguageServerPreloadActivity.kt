package com.ruin.lsp.model

import com.intellij.ide.GeneralSettings
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.application.PreloadingActivity
import com.intellij.openapi.progress.ProgressIndicator
import org.slf4j.LoggerFactory

class LanguageServerPreloadActivity : PreloadingActivity() {
    private val LOG = LoggerFactory.getLogger(LanguageServerPreloadActivity::class.java)

    override fun preload(indicator: ProgressIndicator) {
        LOG.info("Preloading intellij-lsp-server")
        GeneralSettings.getInstance().isShowTipsOnStartup = false

        ApplicationManager.getApplication().executeOnPooledThread {
            val server = LanguageServerServiceImpl.getInstance()

            val future = server.connect(SocketConnectionFactory(8080).open())
        }

        LOG.info("Preloading debug server")
        ApplicationManager.getApplication().executeOnPooledThread {
            val debugServer = DebugServerServiceImpl.getInstance()

            val debugFuture = debugServer.connect(SocketConnectionFactory(8081).open())
        }
    }
}
