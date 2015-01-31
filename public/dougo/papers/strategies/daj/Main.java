import java.io.*;

class Main {
  public static void main(String[] args) throws Exception {
    // A a = A.parse("a b d b e e d b e");
    A a = A.parse(new InputStreamReader(System.in));
    int result = a.countCertainEs();
    System.out.println(result);
  }
}
