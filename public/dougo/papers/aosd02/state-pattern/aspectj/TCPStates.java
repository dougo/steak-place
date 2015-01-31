abstract aspect TCPStates {
  final static int CLOSED = 0, LISTEN = 1, ESTABLISHED = 2;

  pointcut closed():
    if(((TCPConnection) thisJoinPoint.getTarget()).state == CLOSED);
  pointcut listen():
    if(((TCPConnection) thisJoinPoint.getTarget()).state == LISTEN);
  pointcut established():
    if(((TCPConnection) thisJoinPoint.getTarget()).state == ESTABLISHED);

  // initial state
  before(TCPConnection t): call(new()) && target(t) { t.state = CLOSED; }
}
