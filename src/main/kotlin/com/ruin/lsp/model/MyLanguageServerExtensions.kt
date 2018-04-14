package com.ruin.lsp.model

import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.TextDocumentPositionParams
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest
import org.eclipse.lsp4j.jsonrpc.services.JsonSegment
import java.util.concurrent.CompletableFuture

@JsonSegment("idea")
interface MyLanguageServerExtensions {
    @JsonRequest fun implementations(params: TextDocumentPositionParams): CompletableFuture<MutableList<Location>>

    @JsonRequest fun openProjectStructure(params: TextDocumentPositionParams): CompletableFuture<Boolean>

    @JsonRequest fun toggleFrameVisibility(params: TextDocumentPositionParams): CompletableFuture<Boolean>
}

data class ClassHierarchy(val name: String, val children: List<ClassHierarchy>)
