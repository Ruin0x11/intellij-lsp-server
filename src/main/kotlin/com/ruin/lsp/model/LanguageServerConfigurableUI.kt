package com.ruin.lsp.model

import java.awt.BorderLayout
import java.awt.GridBagConstraints
import java.awt.GridBagLayout
import javax.swing.JPanel
import com.intellij.ui.IdeBorderFactory
import com.intellij.ui.PortField
import java.awt.Insets
import java.text.NumberFormat
import javax.swing.JLabel


class LanguageServerConfigurableUI : JPanel() {
    private var portNumberField: PortField
    private val config: LanguageServerConfig = LanguageServerConfig.getInstance()

    val isModified: Boolean
        get() = portNumberField.number != config.portNumber

    init {
        layout = BorderLayout()
        val container: JPanel = this

        val numberFormat = NumberFormat.getIntegerInstance()
        numberFormat.isGroupingUsed = false
        portNumberField = PortField()

        val lspServerSettings = JPanel(GridBagLayout())
        lspServerSettings.border = IdeBorderFactory.createTitledBorder("LSP Server Settings", true)
        val child = JPanel(BorderLayout())
        container.add(child, BorderLayout.NORTH)
        child.add(lspServerSettings, BorderLayout.NORTH)
        val constraints = GridBagConstraints(
            0,
            0,
            1,
            1,
            0.0,
            0.0,
            GridBagConstraints.WEST,
            GridBagConstraints.NONE,
            Insets(0, 0, 0, 0),
            0,
            0)

        lspServerSettings.add(JLabel("Port number:"), constraints)
        constraints.gridx = 1
        constraints.weightx = 1.0
        constraints.fill = GridBagConstraints.HORIZONTAL
        lspServerSettings.add(portNumberField, constraints)
        constraints.gridx = 0
        constraints.gridy = 1
        constraints.weightx = 1.0
    }

    fun apply() {
        config.portNumber = portNumberField.number
    }

    fun reset() {
        portNumberField.number = config.portNumber
    }
}
