package com.ruin.lsp.values
typealias DocumentUri = String
/*
import com.intellij.codeInsight.lookup.LookupElement


data class ClientCapabilities(val workspace: WorkspaceClientCapabilities?,
                              val textDocument: TextDocumentClientCapabilities?)

data class WorkspaceClientCapabilities(val applyEdit: Boolean?,
                                       val workspaceEdit: WorkspaceEditCapability?,
                                       val didChangeConfiguration: DidChangeConfiguration?,
                                       val didChangeWatchedFiles: DidChangeWatchedFiles?,
                                       val symbol: Symbol?,
                                       val executeCommand: ExecuteCommand?)
data class WorkspaceEditCapability(val documentChanges: Boolean?)
data class DidChangeConfiguration(val dynamicRegistration: Boolean?)
data class DidChangeWatchedFiles(val dynamicRegistration: Boolean?)
data class Symbol(val dynamicRegistration: Boolean?,
                  val symbolKind: SymbolKindCapability?)
data class SymbolKindCapability(val valueSet: List<Int>?)
data class ExecuteCommand(val dynamicRegistration: Boolean?)


data class TextDocumentClientCapabilities(val synchronization: Synchronization?,
                                          val completion: Completion?,
                                          val hover: HoverCapability?,
                                          val signatureHelp: SignatureHelp?,
                                          val references: References?,
                                          val documentHighlight: DocumentHighlightCapability?,

                                          // because lsp-mode doesn't follow the spec
                                          // val documentSymbol: DocumentSymbol?,
                                          val symbol: DocumentSymbol?,

                                          val formatting: Formatting?,
                                          val rangeFormatting: RangeFormatting?,
                                          val onTypeFormatting: OnTypeFormatting?,
                                          val definition: Definition?,
                                          val codeAction: CodeAction?,
                                          val codeLens: CodeLens?,
                                          val documentLink: DocumentLink?,
                                          val rename: Rename?)

data class Synchronization(val dynamicRegistration: Boolean?,
                           val willSave: Boolean?,
                           val willSaveWaitUntil: Boolean?,
                           val didSave: Boolean?)

data class Completion(val dynamicRegistration: Boolean?,
                      val completionItem: CompletionItemCapability?,
                      val completionItemKind: CompletionItemKindCapability?,
                      val contextSupport: Boolean?)
data class CompletionItemCapability(val snippetSupport: Boolean?,
                                    val commitCharactersSupport: Boolean?,
                                    val documentationFormat: List<String>?)
data class CompletionItemKindCapability(val valueSet: List<Int>?)

data class HoverCapability(val dynamicRegistration: Boolean?,
                           val contentFormat: List<String>?)

data class SignatureHelp(val dynamicRegistration: Boolean?,
                         val signatureInformation: SignatureInformation?)
data class SignatureInformation(val documentationFormat: List<String>?)

data class References(val dynamicRegistration: Boolean?)
data class DocumentHighlightCapability(val dynamicRegistration: Boolean?)
data class DocumentSymbol(val dynamicRegistration: Boolean?,
                          val symbolKind: SymbolKindCapability)
data class Formatting(val dynamicRegistration: Boolean?)
data class RangeFormatting(val dynamicRegistration: Boolean?)
data class OnTypeFormatting(val dynamicRegistration: Boolean?)
data class Definition(val dynamicRegistration: Boolean?)
data class CodeAction(val dynamicRegistration: Boolean?)
data class CodeLens(val dynamicRegistration: Boolean?)
data class DocumentLink(val dynamicRegistration: Boolean?)
data class Rename(val dynamicRegistration: Boolean?)

data class CompletionOptions(val resolveProvider: Boolean?,
                             val triggerCharacters: List<String>?)
data class SignatureHelpOptions(val triggerCharacters: List<String>?)
data class CodeLensOptions(val resolveProvider: Boolean?)
data class DocumentOnTypeFormattingOptions(val firstTriggerCharacter: String,
                                           val moreTriggerCharacter: List<String>?)
data class DocumentLinkOptions(val resolveProvider: Boolean?)
data class ExecuteCommandOptions(val commands: List<String>)
data class SaveOptions(val includeText: Boolean?)
data class TextDocumentSyncOptions(val openClose: Boolean?,
                                   val change: Int,
                                   val willSave: Boolean?,
                                   val willSaveWaitUntil: Boolean?,
                                   val save: SaveOptions)

data class ServerCapabilities(val textDocumentSync: TextDocumentSyncOptions?,
                              val hoverProvider: Boolean?,
                              val completionProvider: CompletionOptions?,
                              val signatureHelpProvider: SignatureHelpOptions?,
                              val definitionProvider: Boolean?,
                              val referencesProvider: Boolean?,
                              val documentHighlightProvider: Boolean?,
                              val documentSymbolProvider: Boolean?,
                              val workspaceSymbolProvider: Boolean?,
                              val codeActionProvider: Boolean?,
                              val codeLensProvider: CodeLensOptions?,
                              val documentFormattingProvider: Boolean?,
                              val documentRangeFormattingProvider: Boolean?,
                              val documentOnTypeFormattingProvider: DocumentOnTypeFormattingOptions?,
                              val renameProvider: Boolean?,
                              val documentLinkProvider: DocumentLinkOptions?,
                              val executeCommandProvider: ExecuteCommandOptions?,
                              val experimental: Any?)

data class InitializeResult(val capabilities: ServerCapabilities)

data class Position(val line: Int,
                    val character: Int)
data class Range(val start: Position,
                 val end: Position)
data class Location(val uri: DocumentUri,
                    val range: Range)
data class TextDocumentIdentifier(val uri: DocumentUri)
data class TextDocumentPositionParams(val textDocument: TextDocumentIdentifier,
                                      val position: Position)
data class VersionedTextDocumentIdentifier(val uri: DocumentUri,
                                           val version: Int?)

data class WorkspaceEdit(val changes: Map<String, List<TextEdit>>? = null,
                         val documentChanges: List<TextDocumentEdit>?)

data class ApplyWorkspaceEditResponse(val applied: Boolean)

data class TextDocumentContentChangeEvent(val range: Range?,
                                          val rangeLength: Int?,
                                          val text: String)

data class Command(val title: String,
                   val command: String,
                   val arguments: List<Any>?)
data class TextEdit(val range: Range,
                    val newText: String)
data class TextDocumentEdit(val textDocument: VersionedTextDocumentIdentifier,
                            val edits: List<TextEdit>)

data class TextDocumentItem(val uri: DocumentUri,
                            val languageId: String,
                            val version: Int,
                            val text: String)

data class Hover(val contents: MarkedString,
                 val range: Range?)

data class MarkedString(val language: String,
                        val value: String)

data class CompletionContext(val triggerKind: Int?,
                             val triggerCharacters: List<String>?)

class InsertTextFormat {
    companion object {
        const val PLAIN_TEXT = 1
        const val SNIPPET = 2
    }
}
class CompletionItemKind {
    companion object {
        const val TEXT = 1
        const val METHOD = 2
        const val FUNCTION = 3
        const val CONSTRUCTOR = 4
        const val FIELD = 5
        const val VARIABLE = 6
        const val CLASS = 7
        const val INTERFACE = 8
        const val MODULE = 9
        const val PROPERTY = 10
        const val UNIT = 11
        const val VALUE = 12
        const val ENUM = 13
        const val KEYWORD = 14
        const val SNIPPET = 15
        const val COLOR = 16
        const val FILE = 17
        const val REFERENCE = 18
        const val FOLDER = 19
        const val ENUM_MEMBER = 20
        const val CONSTANT = 21
        const val STRUCT = 22
        const val EVENT = 23
        const val OPERATOR = 24
        const val TYPE_PARAMETER = 25
    }
}
data class CompletionList(val isIncomplete: Boolean,
                          val items: List<CompletionItem>)
data class CompletionItem(val label: String,
                          val kind: Int? = null,
                          val detail: String? = null,
                          val documentation: String? = null,
                          val sortText: String? = null,
                          val filterText: String? = null,
                          val insertText: String? = null,
                          val insertTextFormat: Int? = null,
                          val textEdit: TextEdit? = null,
                          val additionalTextEdits: List<TextEdit>? = null,
                          val command: Command? = null,
                          val data: Any? = null) {
    companion object {
        fun from(lookupElement: LookupElement): CompletionItem = CompletionItem(lookupElement.toString())
    }
}

class DocumentHighlightKind {
    companion object {
        const val TEXT = 1
        const val READ = 2
        const val WRITE = 3
    }
}
data class DocumentHighlight(val range: Range,
                             val kind: Int?)

data class SymbolInformation(val name: String,
                             val kind: Int,
                             val location: Location,
                             val containerName: String?)
object SymbolKind {
    const val FILE = 1
    const val MODULE = 2
    const val NAMESPACE = 3
    const val PACKAGE = 4
    const val CLASS = 5
    const val METHOD = 6
    const val PROPERTY = 7
    const val FIELD = 8
    const val CONSTRUCTOR = 9
    const val ENUM = 10
    const val INTERFACE = 11
    const val FUNCTION = 12
    const val VARIABLE = 13
    const val CONSTANT = 14
    const val STRING = 15
    const val NUMBER = 16
    const val BOOLEAN = 17
    const val ARRAY = 18
    const val OBJECT = 19
    const val KEY = 20
    const val NULL = 21
    const val ENUM_MEMBER = 22
    const val STRUCT = 23
    const val EVENT = 24
    const val OPERATOR = 25
    const val TYPE_PARAMETER = 26
}
*/
