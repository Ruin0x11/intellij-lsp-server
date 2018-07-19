# intellij-lsp-server [![AppVeyor Build Status][appveyor-build-status-svg]][appveyor-build-status] [![Travis CI Build Status][travis-build-status-svg]][travis-build-status]
A plugin for IntelliJ IDEA that embeds a Language Server Protocol server, allowing other editors to use IntelliJ's features.

## Requirements
- IntelliJ IDEA 2018.1.1
  + Due to the way the plugin interacts with internal APIs, there currently isn't support for other versions of IDEA. If you're trying to use the plugin with Android Studio, note that the exact same Android support also exists in 2018.1.1.

## Caveats
- Alpha-quality, and probably really unstable.
- Java and Kotlin are currently supported.
- Editing in both IDEA and the LSP client at the same time isn't supported currently.
- The server should work across any LSP client, but some nonstandard features (like using IntelliJ to build and run projects) are only implemented in the Emacs client.

## Features
### Code completion with snippet parameters
Snippet feature is provided by [company-lsp](https://github.com/tigersoldier/company-lsp).

![Code completion with snippet parameters](https://my.mixtape.moe/gkrhey.gif)
### Symbol usage highlighting
Highlights read/write usage.

![Symbol usage highlighting](https://sub.god.jp/f/nieypg.png)
### Find usages
![Find usages](https://my.mixtape.moe/jptlrs.gif)
### Go to definition
Can also find super method if available.

![Go to definition](https://my.mixtape.moe/kklshd.gif)
### Go to implementation
![Go to implementation](https://my.mixtape.moe/zgbddv.gif)
### Diagnostics
Sideline view is provided by [lsp-ui](https://github.com/emacs-lsp/lsp-ui).

![Diagnostics](https://my.mixtape.moe/scoiql.gif)
### Kotlin support
![Kotlin support](https://my.mixtape.moe/clrisu.gif)

## Feature list
| Name                        | Method                            |                    | Emacs function                                         |
| --------------------------- | --------------------------------- | ------------------ | ------------------------------------------------------ |
| Workspace Symbols           | `workspace/symbol`                | :heavy_check_mark: | `xref-find-apropos`                                    |
| Execute Command             | `workspace/executeCommand`        | :heavy_check_mark: |                                                        |
| Diagnostics                 | `textDocument/publishDiagnostics` | :heavy_check_mark: | Used by [lsp-ui](https://github.com/emacs-lsp/lsp-ui). |
| Completion                  | `textDocument/completion`         | :heavy_check_mark: | `complete-symbol`                                      |
| Hover                       | `textDocument/hover`              | :heavy_check_mark: |                                                        |
| Signature Help              | `textDocument/signatureHelp`      | :x:                |                                                        |
| Goto Definition             | `textDocument/definition`         | :heavy_check_mark: | `xref-find-definitions`                                |
| Goto Type Definition        | `textDocument/typeDefinition`     | :heavy_check_mark: | `lsp-goto-type-definition`                             |
| Goto Implementation         | `textDocument/implementation`     | :heavy_check_mark: | `lsp-goto-implementation`                              |
| Find References             | `textDocument/references`         | :heavy_check_mark: | `xref-find-references`                                 |
| Document Highlights         | `textDocument/documentHighlight`  | :heavy_check_mark: |                                                        |
| Document Symbols            | `textDocument/documentSymbol`     | :heavy_check_mark: | `imenu` (with `lsp-imenu`)                             |
| Code Action                 | `textDocument/codeAction`         | :x:                |                                                        |
| Code Lens                   | `textDocument/codeLens`           | :heavy_check_mark: | `lsp-intellij-run-at-point`                            |
| Document Formatting         | `textDocument/formatting`         | :heavy_check_mark: | `lsp-format-buffer`                                    |
| Document Range Formatting   | `textDocument/rangeFormatting`    | :heavy_check_mark: | `indent-region`                                        |
| Document on Type Formatting | `textDocument/onTypeFormatting`   | :x:                |                                                        |
| Rename                      | `textDocument/rename`             | :x:                |                                                        |

### Nonstandard features
| Name                               | Method                        |                              | Emacs function                      |
| ---------------------------------- | ----------------------------- | ---------------------------- | ----------------------------------- |
| Get Run Configurations             | `idea/runConfigurations`      | :leftwards_arrow_with_hook:  |                                     |
| Build Project                      | `idea/buildProject`           | :leftwards_arrow_with_hook:  | `lsp-intellij-build-project`        |
| Run Project                        | `idea/runProject`             | :leftwards_arrow_with_hook:  | `lsp-intellij-run-project`          |
| Indexing Started                   | `idea/indexStarted`           | :arrow_left:                 |                                     |
| Indexing Ended                     | `idea/indexEnded`             | :arrow_left:                 |                                     |
| Build Messages                     | `idea/buildMessages`          | :arrow_left:                 |                                     |
| Build Finished                     | `idea/buildFinished`          | :arrow_left:                 |                                     |

### Commands
| Name                          | Command                 | Emacs function                         |
| ----------------------------- | ----------------------- | -------------------------------------- |
| Open Project Structure        | `openProjectStructure`  | `lsp-intellij-open-project-structure`  |
| Open Run/Debug Configurations | `openRunConfigurations` | `lsp-intellij-open-run-configurations` |
| Toggle IDEA Editor Window     | `toggleFrameVisibility` | `lsp-intellij-toggle-frame-visibility` |

## Installation
### Trying it out
Run `./gradlew runIde` in the repo root to open a testing instance of IDEA.

### Installing the plugin
Run `./gradlew clean buildPlugin` to create the plugin distribution. In IDEA, go to `File -> Settings... -> Plugins -> Install plugin from disk...` and select the `.zip` file that was output inside `build/distributions`.

## Usage
The server will start automatically on TCP port 8080 when the IDE is loaded. You can configure the project SDK inside IDEA before connecting your client or execute the `Open Project Structure` command in the client (`lsp-intellij-open-project-structure` in Emacs) to open the Project Structure window remotely.

To use the server with Emacs/Spacemacs, see the [lsp-intellij](https://www.github.com/Ruin0x11/lsp-intellij) repository.

## Rationale
- I didn't like the latency of `eclim`. `eclim-mode` in emacs has to start a new process to get results from the `eclim` daemon, which takes about 5 seconds per command on my Windows system.
- The [Eclipse Java language server](https://github.com/eclipse/eclipse.jdt.ls) doesn't support Java 7, but projects using it are supported in the latest IDEA.
- [Support for eclim on Windows has been removed.](http://eclim.org/changes.html#jan-01-2018)
- Developer usage of Eclipse itself has fallen over the years.
- The exact same server concept has already existed in the form of [intellivim](https://github.com/dhleong/intellivim), but it supports Vim only through a custom protocol.

<!-- Badges -->
[appveyor-build-status]: https://ci.appveyor.com/project/Ruin0x11/intellij-lsp-server/branch/master
[appveyor-build-status-svg]: https://ci.appveyor.com/api/projects/status/yvuy70pdmfkhn8aw?svg=true
[travis-build-status]: https://travis-ci.org/Ruin0x11/intellij-lsp-server?branch=master
[travis-build-status-svg]: https://travis-ci.org/Ruin0x11/intellij-lsp-server.svg?branch=master
