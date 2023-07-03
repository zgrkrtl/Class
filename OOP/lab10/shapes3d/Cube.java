package shapes3d;


import shapes2d.Square;

public class Cube extends Square{

    public Cube(double side) {
        super(side);
    }

    public double area(){
        return 6 * super.area();
    }

    public double volume(){
        return side * super.area();
    }

    @Override
    public String toString() {
        return "Cube{" +
                "side=" + side +
                '}';
    }
}
