class TCPEstablished extends TCPState {
  static TCPState instance = new TCPEstablished();
  void close(TCPConnection t) {
    // send FIN, receive ACK of FIN
    System.out.println("close() in " + t);
    changeState(t, TCPListen.instance);
  }
  void transmit(TCPConnection t, TCPOctetStream o) {
    System.out.println("transmit(" + o + ") in " + t);
    t.processOctet(o);
  }
}
