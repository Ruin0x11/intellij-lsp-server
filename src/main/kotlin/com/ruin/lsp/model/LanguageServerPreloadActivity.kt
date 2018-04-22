package com.ruin.lsp.model

import com.intellij.ide.GeneralSettings
import com.intellij.openapi.application.PreloadingActivity
import com.intellij.openapi.editor.Document
import com.intellij.openapi.progress.ProgressIndicator
import com.intellij.openapi.vfs.VirtualFile
import org.slf4j.LoggerFactory

class LanguageServerStartupActivity : PreloadingActivity() {
    private val LOG = LoggerFactory.getLogger(LanguageServerStartupActivity::class.java)

    override fun preload(indicator: ProgressIndicator) {
        LOG.info("Preloading intellij-lsp-server")
        GeneralSettings.getInstance().isShowTipsOnStartup = false
        val server = LanguageServerRunnerImpl.getInstance()
        server.run(8080)
    }
}
