package lab9.shapes2d;

public class Circle extends Object {

    protected final int radius;

    public double area() {
        return Math.PI * radius * radius;
    }

    @Override
    public String toString() {
        return "Radius= " + radius;
    }

    public Circle(int radius) {
        this.radius = radius;
    }

    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj instanceof Circle) {
            Circle c = (Circle) obj;
            return radius == c.radius;
        } else
            return false;
    }
}
