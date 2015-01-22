aspect TCPActions extends TCPStates {
  before(TCPConnection t): closed() && call(* activeOpen()) && target(t) {
    // send SYN, receive SYN, ACK, etc.
    System.out.println("activeOpen() in " + t);
  }
  before(TCPConnection t): closed() && call(* passiveOpen()) && target(t) {
    System.out.println("passiveOpen() in " + t);
  }
  before(TCPConnection t): established() && call(* close()) && target(t) {
    // send FIN, receive ACK of FIN
    System.out.println("close() in " + t);
  }
  before(TCPConnection t, TCPOctetStream o):
    established() && call(* transmit(TCPOctetStream)) && target(t) && args(o)
  {
    System.out.println("transmit(" + o + ") in " + t);
    t.processOctet(o);
  }
  before(TCPConnection t): listen() && call(* send()) && target(t) {
    // send SYN, receive SYN, ACK, etc.
    System.out.println("send() in " + t);
  }
}
