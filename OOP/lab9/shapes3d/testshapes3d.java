package lab9.shapes3d;

import jdk.swing.interop.SwingInterOpUtils;
import shapes2d.Square;

import java.util.concurrent.Callable;

public class testshapes3d {
    public static void main(String[] args) {

        Square s = new Square(4);
        Cube c = new Cube(4);
        System.out.println("c.equals : " + c.equals(s));
        System.out.println("s.equals : " + s.equals(c));

        Cylinder cyln = new Cylinder(2, 5);
        System.out.println(cyln.area());
        System.out.println(cyln.volume());
        System.out.println(cyln);
    }
}
