class TCPClosed extends TCPState {
  static TCPState instance = new TCPClosed();
  void activeOpen(TCPConnection t) {
    // send SYN, receive SYN, ACK, etc.
    System.out.println("activeOpen() in " + t);
    changeState(t, TCPEstablished.instance);
  }
  void passiveOpen(TCPConnection t) {
    System.out.println("passiveOpen() in " + t);
    changeState(t, TCPListen.instance);
  }
}
