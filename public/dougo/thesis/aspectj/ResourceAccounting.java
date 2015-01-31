aspect ResourceAccounting of eachJVM() {
  before(): calls(Object, new(..)) {
    System.out.println("Constructing...");
  }
}
