# intellij-lsp-server
A plugin for IntelliJ IDEA that embeds a Language Server Protocol server, allowing other editors to use IntelliJ's features.

## Caveats
- Alpha-quality, and probably really unstable.
- Only targets Java for now, though there is no reason awareness of other languages can't be added.
- Tested primarily with Emacs' [lsp-mode](https://github.com/emacs-lsp/lsp-mode). There are apparently some differences in the way `lsp-mode` implements the specification, so those are currently reflected in the code.
