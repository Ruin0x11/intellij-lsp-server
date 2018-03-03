package org.lsp.javaproject;

public class MyData {
    public enum MyEnum {
        HEY(10), DOOD(20);

        private int val;

        public MyEnum(int val) {
            this.val = val;
        }
    }

    public static final int CONSTANT = 1234;
}
