package org.intellivim.javaproject;

/**
 * @author dhleong
 */
public class SubClass extends SuperClass implements MyInterface {

    static class NestedClass extends Dummy {

    }

    @Override
    public void abstractMethod() {
        System.out.println("Hi.");
    }
}
