/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package com.ruin.intel.actions

import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.actionSystem.AnActionEvent
import com.ruin.intel.model.LanguageServerService

class StartServerAction : AnAction() {
    override fun update(e: AnActionEvent?) {
        e?.presentation?.isEnabled = e?.project != null &&
                !LanguageServerService.getInstance().hasAliveServerProcess()
    }

    override fun actionPerformed(event: AnActionEvent) {
        LanguageServerService.getInstance().startServer()
    }
}