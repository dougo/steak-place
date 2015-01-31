



public aspect ScreenObserving extends Observing of eachJVM() {

  Screen +implements Subject;
  Screen +implements Observer;

  pointcut changes(Subject s): instanceof(s) && 
                               receptions(void Screen.display(String));


  void updateObserver(Subject s, Observer o) {
      ((Screen)o).display("Screen updated because screen displayed.");
  }  

}

