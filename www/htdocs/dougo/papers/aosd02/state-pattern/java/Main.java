class Main {
  public static void main(String args[]) throws Exception {
    TCPConnection passive = new TCPConnection();
    TCPConnection active = new TCPConnection();
    passive.passiveOpen();
    active.activeOpen();
    active.transmit(new TCPOctetStream());
    active.close();
    passive.transmit(new TCPOctetStream()); // does nothing
    passive.send();
    passive.close();
  }
}
