package shapes2d;

public class Circle /*extends Object*/{

    protected double radius;

   /* public Circle() {
        super();
        radius = 1;
        System.out.println("Circle is initalizing");
    }*/

    public Circle(double radius) {
        //super();
        this.radius = radius;
    }

    public double area(){
        return Math.PI * radius * radius;
    }

    @Override
    public String toString() {
        return "Circle{" +
                "radius=" + radius +
                '}';
    }

    public double getRadius() {
        return radius;
    }
}
