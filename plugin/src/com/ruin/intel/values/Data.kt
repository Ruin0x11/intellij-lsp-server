package com.ruin.intel.values


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
                                          val hover: Hover?,
                                          val signatureHelp: SignatureHelp?,
                                          val references: References?,
                                          val documentHighlight: DocumentHighlight?,
                                          val documentSymbol: DocumentSymbol?,
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

data class Hover(val dynamicRegistration: Boolean?,
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







data class CompletionItem(val label: String)