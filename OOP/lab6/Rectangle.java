package lab6;

public class Rectangle {
    private int sideA;
    private int sideB;
    private Point topLeft;

    public Rectangle(Point topLeft, int sideA, int sideB) {
        this.sideA = sideA;
        this.sideB = sideB;
        this.topLeft = topLeft;
    }

    public int area() {
        return sideB * sideA;
    }

    public int perimeter() {
        return 2 * (sideA + sideB);
    }

    public Point[] corners() {
        Point[] corners = new Point[4];
        corners[0] = topLeft;
        corners[1] = new Point(topLeft.getxCoord() + sideA, topLeft.getyCoord());
        corners[2] = new Point(topLeft.getxCoord(), topLeft.getyCoord() - sideB);
        corners[3] = new Point(topLeft.getxCoord() + sideA, topLeft.getyCoord() - sideB);

        return corners;
    }
}
