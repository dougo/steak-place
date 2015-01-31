class TCPConnection {
  TCPState state;
  TCPConnection() { state = TCPClosed.instance; }
  void changeState(TCPState s) {
    System.out.println("Changing state from " + state + " to " + s +
		       " in " + this);
    state = s;
  }

  void transmit(TCPOctetStream o) { state.transmit(this, o); }
  void activeOpen() { state.activeOpen(this); }
  void passiveOpen() { state.passiveOpen(this); }
  void close() { state.close(this); }
  void send() { state.send(this); }
  void acknowledge() { state.acknowledge(this); }
  void synchronize() { state.synchronize(this); }

  void processOctet(TCPOctetStream o) {
    System.out.println("Processing " + o + " in " + this);
  }
}
