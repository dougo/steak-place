aspect TCPStateTransitions extends TCPStates {
  after(TCPConnection t): closed() && call(* activeOpen()) && target(t) {
    t.changeState(ESTABLISHED);
  }
  after(TCPConnection t): closed() && call(* passiveOpen()) && target(t) {
    t.changeState(LISTEN);
  }
  after(TCPConnection t): established() && call(* close()) && target(t) {
    t.changeState(LISTEN);
  }
  after(TCPConnection t): listen() && call(* send()) && target(t) {
    t.changeState(ESTABLISHED);
  }
}
