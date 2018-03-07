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

## Usage
Run `gradle runIde` in the repo root to open a testing instance of IDEA. Alternatively, if you're feeling brave, you can run `gradle buildPlugin` or download a release and install it in your copy of IDEA. The server will start automatically on TCP port 8080 when the IDE is loaded. Be sure the project SDK and any build infrastructure is setup inside IDEA before editing the project over LSP, otherwise things like references and definitions will break.

To use the server with Emacs, [lsp-mode](https://github.com/emacs-lsp/lsp-mode) is required. First load `lsp-mode` and the `lsp-intellij.el` file in your config, then put the following hook afterward:
```emacs-lisp
(with-eval-after-load 'lsp-mode
  (require 'lsp-intellij)
  (add-hook 'java-mode-hook #'lsp-intellij-enable))
```
Then visit a `.java` file tracked by a project you've opened in IDEA.

## Caveats
- Alpha-quality, and probably really unstable.
- Only targets Java for now, though there is no reason awareness of other languages can't be added.
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
