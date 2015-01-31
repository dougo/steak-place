aspect TCPState {
  int TCPConnection.state;
  void TCPConnection.changeState(int s) {
    System.out.println("Changing state from " + state + " to " + s +
		       " in " + this);
    state = s;
  }
}
