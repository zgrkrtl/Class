package lab9.shapes3d;

import shapes2d.Circle;

public class Cylinder extends Circle {

    private int height;

    public Cylinder(int radius, int height) {
        super(radius);
        this.height = height;
    }

    public double area() {
        return 2 * super.area() + height * 2 * Math.PI * radius;
    }

    public double volume() {
        return height * super.area();
    }

    @Override
    public String toString() {
        return super.toString() + " height= " + height;

    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Cylinder) {
            Cylinder cy = (Cylinder) obj;
            return height == cy.height && radius == cy.radius;

        }
        return false;
    }
}