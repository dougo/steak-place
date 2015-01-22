abstract class TCPState {
  void transmit(TCPConnection t, TCPOctetStream o) { }
  void activeOpen(TCPConnection t) { }
  void passiveOpen(TCPConnection t) { }
  void close(TCPConnection t) { }
  void acknowledge(TCPConnection t) { }
  void synchronize(TCPConnection t) { }
  void send(TCPConnection t) { }
  void changeState(TCPConnection t, TCPState s) {
    t.changeState(s);
  }
}
