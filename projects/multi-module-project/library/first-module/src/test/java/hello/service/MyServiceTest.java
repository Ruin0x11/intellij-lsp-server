package hello.service;

import org.junit.Test;
import org.junit.runner.RunWith;
public class MyServiceTest {

    private MyService myService;

    @Test
    public void contextLoads() {
        assert(myService.message() != null);
    }

}
