package com.ruin.lsp.commands.document.find

import com.ruin.lsp.DUMMY_FILE_PATH
import com.ruin.lsp.JAVA_PROJECT
import com.ruin.lsp.KOTLIN_PROJECT
import com.ruin.lsp.forKotlin
import org.eclipse.lsp4j.Position

class FindUsagesCommandTestCaseKt : FindUsagesCommandTestBase() {
    override val projectName: String
        get() = KOTLIN_PROJECT

    fun `test finds no usages`() = checkFindsNothing(forKotlin(DUMMY_FILE_PATH),
        Position(36, 22))

    fun `test finds multiple usages`() = checkFindsLocation(forKotlin(DUMMY_FILE_PATH),
        Position(25, 22), "Dummy.kt", Position(41, 8))
}
