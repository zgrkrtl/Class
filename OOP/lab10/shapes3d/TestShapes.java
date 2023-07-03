package shapes3d;

public class TestShapes {

    public static void main(String[] args){
        Cylinder cy = new Cylinder(5,6);

        System.out.println(cy);

        cy.volume();

        Cube cu = new Cube(7);

        System.out.println(cu);
    }
}
