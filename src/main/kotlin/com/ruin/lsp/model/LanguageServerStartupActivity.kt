package com.ruin.lsp.model

import com.intellij.openapi.application.PreloadingActivity
import com.intellij.openapi.progress.ProgressIndicator

class LanguageServerStartupActivity : PreloadingActivity() {
    override fun preload(indicator: ProgressIndicator) {
        LOG.info("Preloading intellij-lsp-server")
        val service = LanguageServerService.getInstance()
        service.startServer()
    }
}
