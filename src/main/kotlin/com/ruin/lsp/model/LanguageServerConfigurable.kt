package com.ruin.lsp.model

import com.intellij.openapi.options.SearchableConfigurable
import javax.swing.JComponent

class LanguageServerConfigurable : SearchableConfigurable {
    private var myGUI: LanguageServerConfigurableUI? = null
    private var myConfig: LanguageServerConfig? = null

    override fun disposeUIResources() {
        myGUI = null
    }

    override fun enableSearch(option: String?): Runnable? = null

    override fun isModified() = myGUI?.isModified == true

    override fun getId() = helpTopic

    override fun getHelpTopic() = "com.ruin.lsp.settings"

    override fun getDisplayName() = "LSP Server"

    override fun apply() = myGUI?.apply() ?: Unit

    override fun reset() = myGUI?.reset() ?: Unit

    override fun createComponent(): JComponent {
        myGUI = LanguageServerConfigurableUI()
        return myGUI!!
    }
}
