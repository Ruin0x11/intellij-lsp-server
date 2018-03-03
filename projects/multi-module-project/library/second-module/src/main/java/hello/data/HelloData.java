package hello.data;

public class HelloData {
  public enum HelloEnum {
    HELLO("Hello!"), HOLA("¡Hola!"), BONJOUR("Bonjour!"), KONNICHIWA("こんにちは！");

    public String message;

    HelloEnum(String message) {
      this.message = message;
    }
  }

  public static final String GREETINGS = "Greetings!";
}
