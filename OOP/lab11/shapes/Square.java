package shapes;

public class Square extends Shape {
    private double side;

    public Square(double side) {
        super();
        this.side = side;
    }

    public double area() {
        return Math.pow(side, 2);
    }

}
