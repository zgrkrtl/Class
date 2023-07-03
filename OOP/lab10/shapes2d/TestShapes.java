package shapes2d;

public class TestShapes {

    public static void main(String[] args){
        Circle c = new Circle(5);
        c.area();

        System.out.println("Circle c: " + c);

        Square s = new Square(6);

        System.out.println("Square s :" + s);
    }
}
