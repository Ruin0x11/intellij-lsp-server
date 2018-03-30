package com.ruin.lsp.model;

import com.intellij.openapi.project.Project
import org.eclipse.lsp4j.ClientCapabilities

class Context {
    var clientCapabilities: ClientCapabilities? = null
    var rootProject: Project? = null
}
