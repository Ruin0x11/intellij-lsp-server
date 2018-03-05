package com.ruin.lsp


import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.Range

fun range(startLine: Int, startChar: Int, endLine: Int, endChar: Int): Range =
    Range(Position(startLine, startChar), Position(endLine, endChar))
