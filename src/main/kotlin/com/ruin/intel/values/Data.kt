package com.ruin.intel.values

typealias DocumentUri = String
typealias MarkedString = String

data class ClientCapabilities(val workspace: WorkspaceClientCapabilities?,
                              val textDocument: TextDocumentClientCapabilities?)

data class WorkspaceClientCapabilities(val applyEdit: Boolean?,
                                       val workspaceEdit: WorkspaceEdit?,
                                       val didChangeConfiguration: DidChangeConfiguration?,
                                       val didChangeWatchedFiles: DidChangeWatchedFiles?,
                                       val symbol: Symbol?,
                                       val executeCommand: ExecuteCommand?)
data class WorkspaceEdit(val documentChanges: Boolean?)
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
                                          val documentHighlight: DocumentHighlight?,

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
data class DocumentHighlight(val dynamicRegistration: Boolean?)
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

data class CancelParams(val id: String)

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

data class TextDocumentContentChangeEvent(val range: Range?,
                                          val rangeLength: Int?,
                                          val text: String)

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

data class CompletionContext(val triggerKind: Int?,
                             val triggerCharacters: List<String>?)
data class CompletionItem(val label: String)

