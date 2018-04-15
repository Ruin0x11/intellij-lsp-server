package org.lsp.kotlinproject

import org.lsp.kotlinproject.Dummy

/**
 * Some dummy class for testing purposes
 * @author dhleong
 */
open class Dummy {
    private var thingy = 42
    fun boring() {
        println("Hi")
        List<String>()
        Dummy().fluid().boring()
        val list = List<String>()
        list.add("hi")
        notBoring(42)
        thingy = 12
    }

    /** I promise it's not boring  */
    fun notBoring(number: Int) {
        val problem: Problematic
    }

    internal fun notBoring(number: Int, foo: String) {}

    fun fluid(): Dummy {
        return this
    }

    /* Constructors last so existing tests with offsets aren't broken */

    internal constructor() {}
    internal constructor(number: Int) {}
    internal constructor(string: String) {}
    internal constructor(number: Int, andString: String) {}

    /* New code for testing params */

    internal fun moreBoring() {
        notBoring(42, "foo")
        notBoring(answerQuestion("life"), "universe")
    }

    companion object {

        internal fun answerQuestion(question: String): Int {
            return 42
        }

        internal fun autoImportTestMethod(): Int {
            Lin // java.util.LinkedHashMap
        }

        internal fun autoImportTestMethod2(): Int {
            Arr // java.util.ArrayList
        }
    }

    internal fun withClosure(closure: (Int) -> Unit) {
        closure(42)
    }
}

fun outsidePackage(i: Int) {
    println(i)
}

fun javaPropertyTestMethod() {
    val getSet = GetSet()
    getSet.value
    getSet.objec
}
