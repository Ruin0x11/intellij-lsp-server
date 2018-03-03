package hello.app;

import hello.service.ServiceProperties;
import org.junit.Test;

import hello.service.MyService;
import hello.data.HelloData.HelloEnum;

public class DemoApplicationTest {

    private MyService myService;

    @Test
    public void contextLoads() {
        ServiceProperties serviceProperties = new ServiceProperties();
        serviceProperties.setMessage(HelloEnum.KONNICHIWA.message);
        myService = new MyService(serviceProperties);
        assert(myService != null);
    }
}
