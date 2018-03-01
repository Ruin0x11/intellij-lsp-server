(I basically ripped off the basic project structure from `intellij-rust`, so the build process is similar. Most of the following is appropriated from their `CONTRIBUTING.md`.)

# Kotlin

We use [Kotlin] language for the plugin. If you can program in Java, you should
be able to read and write Kotlin code right away. Kotlin is deeply similar to
Java, but has less verbose syntax and better safety. It also shares some
characteristics with Rust: type inference, immutability by default, no null
pointers (apart from those that come from Java).

[Kotlin]: https://kotlinlang.org/

# Getting started

## Clone

```
git clone https://github.com/Ruin0x11/intellij-lsp-server.git
cd intellij-lsp-server
```


## Building

We use gradle to build the plugin. It comes with a wrapper script (`gradlew` in
the root of the repository) which downloads appropriate version of gradle
automatically as long as you have JDK installed.

Common tasks are

  - `./gradlew :build` -- fully build plugin and create an archive at
    `build/distributions` which can be installed into IntelliJ IDEA via `Install
    plugin from disk...` action found in `Settings > Plugins`.

  - `./gradlew :runIde` -- run a development IDE with the plugin installed.

  - `./gradlew :test` -- Tests.


## Development in Intellij IDEA

You can get the latest Intellij IDEA Community Edition
[here](https://www.jetbrains.com/idea/download/), it is free.

Import the plugin project as you would do with any other gradle based project.
For example, <kbd>Ctrl + Shift + A</kbd>, `Import project` and select `build.gradle.kts` from
the root directory of the plugin.

Try executing `./gradlew :test` first, to download all necessary dependencies 
and launch all code generation tasks. Unfortunately during import IDEA may delete 
`.idea/runConfigurations`, just revert changes in the directory if this happens.


## Troubleshooting

If you try running the tests in IDEA but get "Class not found" errors, check the `out` 
directory in the repo root and see where the classes are being output. It could either be 
`out/test/classes` or `out/test`. Go to `Project Structure...` and set the module output
directories for all modules so they match. It's probably a bug in the Gradle import.
