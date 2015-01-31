class TCPListen extends TCPState {
  static TCPState instance = new TCPListen();
  void send(TCPConnection t) {
    // send SYN, receive SYN, ACK, etc.
    System.out.println("send() in " + t);
    t.changeState(TCPEstablished.instance);
  }  
}
