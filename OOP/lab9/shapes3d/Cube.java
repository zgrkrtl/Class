package lab9.shapes3d;

import shapes2d.Square;

public class Cube extends Square {

    public Cube(int side) {
        super(side);
    }

    @Override
    public double area() {
        return 6 * super.area();

    }

    public double volume() {
        return side * super.area();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Cube) {
            Cube c = (Cube) obj;
            return side == side;

        }
        return false;
    }
}
