package lab6;

public class Circle {
    private int radius;
    private Point center;
    static final double PI = 3.14;

    public Circle(int radius, Point center) {
        this.radius = radius;
        this.center = center;
    }

    public double area() {

        return PI * radius * radius;

    }

    public double perimeter() {
        return 2 * PI * radius;
    }

    public boolean intersect(Circle circle) {

        return radius + circle.radius >= center.distanceFromPoint(circle.center);
    }

}
