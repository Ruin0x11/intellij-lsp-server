# intellij-lsp-server [![AppVeyor Build Status][appveyor-build-status-svg]][appveyor-build-status]
A plugin for IntelliJ IDEA that embeds a Language Server Protocol server, allowing other editors to use IntelliJ's features.

## Features
### Code completion with snippet parameters
Snippet feature is provided by [company-lsp](https://github.com/tigersoldier/company-lsp).

![Code completion with snippet parameters](https://sub.god.jp/f/uecray.gif)
### Symbol usage highlighting
Highlights read/write usage.

![Symbol usage highlighting](https://sub.god.jp/f/nieypg.png)
### Find usages
![Find usages](https://sub.god.jp/f/aeitpo.gif)
### Go to definition
Can also find super method if available.

![Go to definition](https://sub.god.jp/f/lcocla.gif)
### Go to implementation
![Go to implementation](https://sub.god.jp/f/uighbt.gif)
### Diagnostics
Sideline view is provided by [lsp-ui](https://github.com/emacs-lsp/lsp-ui).

![Diagnostics](https://sub.god.jp/f/ianlhr.gif)
### Kotlin support
![Kotlin support](https://sub.god.jp/f/necrpl.gif)

## Feature list
| Name                        | Method                            |                    | Emacs function                                         |
| ----                        | -----------------------------     | ------------------ | -----------------------------------                    |
| Workspace Symbols           | `workspace/symbol`                | :heavy_check_mark: | `xref-find-apropos`                                    |
| Execute Command             | `workspace/executeCommand`        | :heavy_check_mark: |                                                        |
| Diagnostics                 | `textDocument/publishDiagnostics` | :heavy_check_mark: | Used by [lsp-ui](https://github.com/emacs-lsp/lsp-ui). |
| Completion                  | `textDocument/completion`         | :heavy_check_mark: | `complete-symbol`                                      |
| Hover                       | `textDocument/hover`              | :heavy_check_mark: |                                                        |
| Signature Help              | `textDocument/signatureHelp`      | :x:                |                                                        |
| Goto Definition             | `textDocument/definition`         | :heavy_check_mark: | `xref-find-definitions`                                |
| Goto Type Definition        | `textDocument/typeDefinition`     | :x:                |                                                        |
| Find References             | `textDocument/references`         | :heavy_check_mark: | `xref-find-references`                                 |
| Document Highlights         | `textDocument/documentHighlight`  | :heavy_check_mark: |                                                        |
| Document Symbols            | `textDocument/documentSymbol`     | :heavy_check_mark: | `imenu` (with `lsp-imenu`)                             |
| Code Action                 | `textDocument/codeAction`         | :heavy_check_mark: | `lsp-intellij-run-at-point`                             |
| Code Lens                   | `textDocument/codeLens`           | :x:                |                                                        |
| Document Formatting         | `textDocument/formatting`         | :heavy_check_mark: | `lsp-format-buffer`                                    |
| Document Range Formatting   | `textDocument/rangeFormatting`    | :heavy_check_mark: | `indent-region`                                        |
| Document on Type Formatting | `textDocument/onTypeFormatting`   | :x:                |                                                        |
| Rename                      | `textDocument/rename`             | :x:                |                                                        |

### Nonstandard features
| Name                               | Method                        |                             | Emacs function                         |
| ---------------------------------- | ----------------------------- | ---------------------------- | -----------------------------------    |
| Find Implementations               | `idea/implementations`        | :leftwards_arrow_with_hook:  | `lsp-intellij-find-implementations`    |
| Get Run Configurations             | `idea/runConfigurations`      | :leftwards_arrow_with_hook:  |                                                       |
| Build Project                      | `idea/buildProject`           | :leftwards_arrow_with_hook:  | `lsp-intellij-build-project`                           |
| Run Project                        | `idea/runProject`             | :leftwards_arrow_with_hook:  | `lsp-intellij-run-project`                             |
| Indexing Started                   | `idea/indexStarted`           | :arrow_left:                 |                                                       |
| Indexing Ended                     | `idea/indexEnded`             | :arrow_left:                 |                                                       |
| Build Messages                     | `idea/buildMessages`          | :arrow_left:                 |                                                       |
| Build Finished                     | `idea/buildFinished`          | :arrow_left:                 |                                                       |

### Commands
| Name                          | Command                 | Emacs function                         |
| ----------------------------- | ----------------------- | -------------------------------------- |
| Open Project Structure        | `openProjectStructure`  | `lsp-intellij-open-project-structure`  |
| Open Run/Debug Configurations | `openRunConfigurations` | `lsp-intellij-open-run-configurations` |
| Toggle IDEA Editor Window     | `toggleFrameVisibility` | `lsp-intellij-toggle-frame-visibility` |

## Usage
Run `gradle runIde` in the repo root to open a testing instance of IDEA. Alternatively, if you're feeling brave, you can run `gradle buildPlugin` or download a release and install it in your copy of IDEA. The server will start automatically on TCP port 8080 when the IDE is loaded. Be sure the project SDK and any build infrastructure is setup inside IDEA before editing the project over LSP, otherwise things like references and definitions will break.

To use the server with Emacs, [lsp-mode](https://github.com/emacs-lsp/lsp-mode) is required. First load `lsp-mode` and the `lsp-intellij.el` file in your config, then put the following hook afterward:
```emacs-lisp
(with-eval-after-load 'lsp-mode
  (require 'lsp-intellij)
  (add-hook 'java-mode-hook #'lsp-intellij-enable))
```
Then visit a `.java` file tracked by a project you've opened in IDEA. You can do the same for Kotlin by installing `kotlin-mode`, then adding another hook for `lsp-intellij-enable` in `kotlin-mode-hook`.

For the extra features shown in the demonstration, `lsp-ui` and `company-lsp` are required. Here are the respective config options for each.
```emacs-lisp
(require 'lsp-ui)
(add-hook 'lsp-after-open-hook #'lsp-ui-mode)

(require 'company-lsp)
(setq company-lsp-enable-snippet t
      company-lsp-cache-candidates t)
(push 'company-lsp company-backends)
(push 'java-mode company-global-modes)
(push 'kotlin-mode company-global-modes) ;; if using Kotlin
```

### Spacemacs

For Spacemacs you can put the configuration into the private layer (recommended as it's still in the early stages). Minimal required configuration to do so:

* copy `lsp-intellij.el` into `~/.emacs.d/private/lsp-intellij/local/lsp-intellij/`
* create `packages.el` in `~/.emacs.d/private/lsp-intellij/` with following content as bare minimum:

```emacs-lisp
(defconst intellij-lsp-packages
  '(
    lsp-mode
    (lsp-intellij :location local)
    ))

(defun intellij-lsp/init-lsp-mode ()
  (use-package lsp-mode))

(defun intellij-lsp/init-lsp-intellij ()
  (with-eval-after-load 'lsp-mode
    (use-package lsp-intellij)
    (add-hook 'java-mode-hook #'lsp-intellij-enable)))

```

Then you should have a similar structure to the following:


```
➜  ~ git:(master) ✗ tree ~/.emacs.d/private/lsp-intellij
/home/user/.emacs.d/private/lsp-intellij
├── local
│   └── lsp-intellij
│       └── lsp-intellij.el
└── packages.el

```

## Caveats
- Alpha-quality, and probably really unstable.
- Tested primarily with Emacs' [lsp-mode](https://github.com/emacs-lsp/lsp-mode). There are apparently some differences in the way `lsp-mode` implements the specification, so those are currently reflected in the code.
- Editing in both IDEA and the LSP client at the same time isn't supported currently.

## Rationale
- I didn't like the latency of `eclim`. `eclim-mode` in emacs has to start a new process to get results from the `eclim` daemon, which takes about 5 seconds per command on my Windows system.
- The [Eclipse Java language server](https://github.com/eclipse/eclipse.jdt.ls) doesn't support Java 7, but projects using it are supported in the latest IDEA.
- [Support for eclim on Windows has been removed.](http://eclim.org/changes.html#jan-01-2018)
- Developer usage of Eclipse itself has fallen over the years.
- The exact same server concept has already existed in the form of [intellivim](https://github.com/dhleong/intellivim), but it supports Vim only through a custom protocol.

<!-- Badges -->
[appveyor-build-status]: https://ci.appveyor.com/project/Ruin0x11/intellij-lsp-server/branch/master
[appveyor-build-status-svg]: https://ci.appveyor.com/api/projects/status/yvuy70pdmfkhn8aw?svg=true
