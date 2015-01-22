class CountingVisitor {
  int r;
  void start() { r = 0; }
  void before(E e) { r++; }
  int getReturnValue() { return r; }
}
