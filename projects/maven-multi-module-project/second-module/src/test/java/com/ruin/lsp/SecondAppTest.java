package com.ruin.lsp;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class SecondAppTest
    extends TestCase
{
    public SecondAppTest( String testName )
    {
        super( testName );
    }

    public static Test suite()
    {
        return new TestSuite( SecondAppTest.class );
    }

    public void testApp()
    {
        assertTrue( true );
    }
}
