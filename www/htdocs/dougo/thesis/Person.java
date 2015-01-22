class Person {
  public static void main(String args[]) {
    Knight gandalf = new Knight("Ian", "McKellan");
    System.out.println(gandalf.fullName());
  }
  String fname, lname;
  Person(String f, String l) { fname = f; lname = l; }
  String fullName() { return fname + " " + lname; }
}
class Knight extends Person {
  Knight(String f, String l) { super(f, l); }
  String fullName() { return "Sir " + super.fullName(); }
}
aspect Logging {
  before(Person p): call(* *(..)) && target(p) &&
                    !cflow(execution(* before(..))) {
    System.out.println(thisEnclosingJoinPointStaticPart);
    System.out.println(thisJoinPointStaticPart);
    System.out.println(p.fullName() + " received message " +
		       thisJoinPoint.getSignature());
  }
}
