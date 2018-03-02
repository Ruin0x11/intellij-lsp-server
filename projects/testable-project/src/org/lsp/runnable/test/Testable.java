
package org.lsp.runnable.test;

import junit.framework.TestCase;

public class Testable extends TestCase {

    public void test1() {
        try { Thread.sleep(2000); } catch (Exception e) { e.printStackTrace(); }
    }

    public void test2() {
        try { Thread.sleep(3000); } catch (Exception e) { e.printStackTrace(); }
        fail("test2 failed");
    }
}
