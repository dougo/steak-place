
import java.awt.Color;

public class Main {

  public static void main(String argv[]){
      
    Point p=new Point(5,5,Color.blue);

    Screen s1 = new Screen();
    Screen s2 = new Screen();
    
    Screen s3 = new Screen();
    Screen s4 = new Screen();

    Screen s5 = new Screen();

    ColorObserving.aspectOf().addObserver(p, s1);    
    ColorObserving.aspectOf().addObserver(p, s2);

    CoordinateObserving.aspectOf().addObserver(p, s3);   
    CoordinateObserving.aspectOf().addObserver(p, s4);
    
    ScreenObserving.aspectOf().addObserver(s2, s5);
    ScreenObserving.aspectOf().addObserver(s4, s5);

    p.setColor(Color.red);
    p.setX(4);
  }
}

