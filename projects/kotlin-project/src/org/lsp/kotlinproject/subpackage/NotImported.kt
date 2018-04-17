package org.lsp.kotlinproject.subpackage

import org.lsp.javaproject.Something

/**
 * Dummy class for testing imports
 */
class NotImported {

    fun notUsed() {}

    companion object {

        fun doSomething(): Something {
            return Something()
        }
    }
}
