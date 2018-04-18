package org.lsp.kotlinproject


/**
 * @author dhleong
 */
class SubClass : SuperClass(), MyInterface {

    internal class NestedClass : Dummy()

    @Override
    override fun abstractMethod() {
        println("Hi.")
    }
}
