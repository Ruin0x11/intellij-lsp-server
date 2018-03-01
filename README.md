# intellij-lsp-server
A plugin for IntelliJ IDEA that embeds a Language Server Protocol server, allowing other editors to use IntelliJ's features.

## Caveats
- Alpha-quality, and probably really unstable.
- Only targets Java for now, though there is no reason awareness of other languages can't be added.
- Tested primarily with Emacs' [lsp-mode](https://github.com/emacs-lsp/lsp-mode). There are apparently some differences in the way `lsp-mode` implements the specification, so those are currently reflected in the code.

## Rationale
- I didn't like the latency of `eclim`. `eclim-mode` in emacs has to start a new process to get results from the `eclim` daemon, which takes about 5 seconds per command on my Windows system.
- The [Eclipse Java language server](https://github.com/eclipse/eclipse.jdt.ls) doesn't support Java 7, but projects using it are supported in the latest IDEA.
- [Support for eclim on Windows has been removed.](http://eclim.org/changes.html#jan-01-2018)
- Developer usage of Eclipse itself has fallen over the years.
- The exact same server concept has already existed in the form of [intellivim](https://github.com/dhleong/intellivim), but it supports Vim only through a custom protocol.
