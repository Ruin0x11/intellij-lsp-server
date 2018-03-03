package hello.app;

import hello.data.HelloData;
import hello.data.HelloData.HelloEnum;
import hello.service.MyService;

public class DemoApplication {

    private final MyService myService;

    public DemoApplication(MyService myService) {
        this.myService = myService;
    }

    public String home() {
        return myService.message() + " " + HelloEnum.KONNICHIWA.message;
    }

    public static void main(String[] args) {
      System.out.println(HelloData.GREETINGS + " Welcome!");
    }
}
