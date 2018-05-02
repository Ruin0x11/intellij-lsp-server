package com.ruin.lsp.model

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.*
import com.intellij.util.xmlb.XmlSerializerUtil

@State(
    name = "LanguageServerConfig",
    storages = [(Storage(value = "intellij-lsp-server.xml"))]
)
class LanguageServerConfig : PersistentStateComponent<LanguageServerConfig> {
    val DEFAULT_PORT_NUMBER = 8080

    var portNumber = DEFAULT_PORT_NUMBER

    override fun getState() = this

    override fun loadState(languageServerConfig: LanguageServerConfig) =
        XmlSerializerUtil.copyBean(languageServerConfig, this)

    companion object {
        fun getInstance(): LanguageServerConfig {
            return ApplicationManager.getApplication().getComponent(LanguageServerConfig::class.java)
        }
    }
}
