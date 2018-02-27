package org.intellivim.looping;

public class LoopingMain {
    public static final void main(String[] args) {

        int i = 0;
        while (true) {
            System.out.println("Loop #" + (++i));

            try {
                Thread.sleep(500);
            } catch (Throwable e) {}
        }
    }
}
