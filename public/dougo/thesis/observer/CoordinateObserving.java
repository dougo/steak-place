



public aspect CoordinateObserving extends Observing of eachJVM() {

  Point  +implements Subject;
  Screen +implements Observer;

  pointcut changes(Subject s): instanceof(s) &&
                               (receptions(void Point.setX(int)) ||
				receptions(void Point.setY(int)) );

  void updateObserver(Subject s, Observer o) {
      ((Screen)o).display("Screen updated because coordinates changed.");
  }

}
