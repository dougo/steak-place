class TCPConnection {
  void transmit(TCPOctetStream o) { }
  void activeOpen() { }
  void passiveOpen() { }
  void close() { }
  void send() { }
  void acknowledge() { }
  void synchronize() { }

  void processOctet(TCPOctetStream o) {
    System.out.println("Processing " + o + " in " + this);
  }
}
