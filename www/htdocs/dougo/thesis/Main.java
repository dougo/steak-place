class Main {
  static aspect fooBody {
    void Main.foo() { }
    void around(): target(Main) && call(void foo()) {
      System.out.println("Hello, sailor!");
    }
  }

  public static void main(String args[]) {
    new Main().foo();
  }
}
