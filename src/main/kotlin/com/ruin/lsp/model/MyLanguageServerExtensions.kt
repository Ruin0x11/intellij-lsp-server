package com.ruin.lsp.model

import com.ruin.lsp.values.DocumentUri
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.TextDocumentIdentifier
import org.eclipse.lsp4j.TextDocumentPositionParams
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest
import org.eclipse.lsp4j.jsonrpc.services.JsonSegment
import java.util.concurrent.CompletableFuture

@JsonSegment("idea")
interface MyLanguageServerExtensions {
    @JsonRequest fun implementations(params: TextDocumentPositionParams): CompletableFuture<MutableList<Location>>

    @JsonRequest fun setProjectJdk(params: SetProjectJDKParams): CompletableFuture<SetProjectJDKResult>
}

enum class JdkKind(val value: Int) {
    JDK(1), INTELLIJ(2)
}
data class SetProjectJDKParams(val textDocument: TextDocumentIdentifier, val jdkRootUri: String, val kind: JdkKind)
data class SetProjectJDKResult(val success: Boolean, val version: String?)
