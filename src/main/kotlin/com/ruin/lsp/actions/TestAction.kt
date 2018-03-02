/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package com.ruin.lsp.actions

import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.actionSystem.PlatformDataKeys
import com.intellij.openapi.ui.Messages

class TestAction : AnAction() {
    override fun actionPerformed(event: AnActionEvent) {
        val project = event.getData(PlatformDataKeys.PROJECT)
        Messages.showMessageDialog(project, "Hello world!", "Greeting", Messages.getInformationIcon())
    }
}
