/*   Generated by AspectJ version 1.0.4 */
class Knight extends Person {
  static org.aspectj.runtime.reflect.Factory ajc$JPF;
  private static org.aspectj.lang.JoinPoint.StaticPart fullName$ajcjp1;
  Knight(String f, String l) {
    super(f, l);
    ;
  } 
  String fullName() {
    {
      if (!Logging.cflow$ajc0.isValid()) Logging.aspectInstance.before0$ajc(this, 
          Knight.fullName$ajcjp1);
    } 
    return this.fullName$ajcPostCall();
  } 

  String fullName$ajcPostCall() {
    Logging.cflow$ajc0.push(new org.aspectj.runtime.CFlow());
    try {
      return "Sir " + super.fullName$ajcPostCall();
    } finally {
      Logging.cflow$ajc0.pop();
    } 
  } 

  static {
    Knight.ajc$JPF = new org.aspectj.runtime.reflect.Factory("Person.java", Knight.class);
    Knight.fullName$ajcjp1 = Knight.ajc$JPF.makeSJP("method-call", 
        Knight.ajc$JPF.makeMethodSig("0-fullName-Knight----java.lang.String-"), null);
  } 

} 