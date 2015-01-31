
import java.awt.Color;
/**
 *
 * Class Point is a simple subject with two properties.
 */
public class Point{
  int x,y;
  Color color=Color.blue;

  public Point(int x, int y, Color color) {
    this.x=x;
    this.y=y;
    this.color=color;
  }

  public int getX(){ return x;}
  public void setX(int x) { this.x=x;}

  public int getY(){ return y;}
  public void setY(int y) { this.y=y;}

  public Color getColor(){ return color;}
  public void setColor(Color color){ this.color=color;}
}
