
import java.awt.Color;


public aspect ColorObserving extends Observing of eachJVM() {

  Point  +implements Subject;
  Screen +implements Observer;

  pointcut changes(Subject s): instanceof(s) && 
                               receptions(void Point.setColor(Color));


  void updateObserver(Subject s, Observer o) {
      ((Screen)o).display("Screen updated because color changed.");
  }

}
