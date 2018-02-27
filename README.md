# LSP

## Contributing
If you try running the tests in IDEA but get "Class not found" errors, check the "out" directory for where classes are
output. It could either be "out/test/classes" or "out/test". Go to `Project Structure...` and set the module output
directories for all modules so they match. It's probably a bug in the Gradle import.

## Bugs
- Opening a project through the LSP after it's already been opened before the LSP was started causes an error about the
project being disposed already.
- `Disposer` doesn't release editors created by `createEditor` for some reason, so they have to be released manually
every time.
