import java.util.*;

class Hello {
  static Object o1 = new Object();
  static {
    System.out.println("o1 = " + o1);
    Object o2 = new Object();
    System.out.println("o2 = " + o2);
  }
  Object o3 = new Object();
  {
    System.out.println("o3 = " + o3);
    Object o4 = new Object();
    System.out.println("o4 = " + o4);
  }
  Hello() {
    Object o5 = new Object();
    System.out.println("o5 = " + o5);
  }
  public static void main(String args[]) {
    Hello h = new Hello();
    System.out.println("h = " + h);
    // String hello[] = new String[] { "h", "e", "l", "l", "o" };
    // System.out.println(h);
  }
  // String hello[] = new String[] { "h", "e", "l", "l", "o" };
  // public String toString() { return Arrays.asList(hello).toString(); }
}
