package com.ruin.lsp

import com.ruin.lsp.util.resolvePsiFromUri

class GotoFileInsideJarTest : UsableSdkTestCase() {
    fun `test resolves a sources jar`() {
        val project = prepareProject(JAVA_PROJECT)
        val tempDir = System.getProperty("java.io.tmpdir")
        val psi = resolvePsiFromUri(project, tempDir, tempDir)
        assert(false)
    }

    fun `test provides decompiled class file when no source`() {
        val project = prepareProject(JAVA_PROJECT)
        val tempDir = System.getProperty("java.io.tmpdir")
        val psi = resolvePsiFromUri(project, tempDir, tempDir)
        assert(false)
    }
}
