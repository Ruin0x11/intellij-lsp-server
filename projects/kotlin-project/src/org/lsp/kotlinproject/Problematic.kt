package org.lsp.kotlinproject

/**
 * Class with intentional problems for testing
 */
class Problematic {

    fun foo() {
        val obj: NotImported
        val other: AlsoNotImported
    }

}
