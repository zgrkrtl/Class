package lab9.shapes2d;

public class testshapes2d {
    public static void main(String[] args) {
        Circle crc1 = new Circle(3);
        crc1 = new Circle(5);
        Circle crc2 = new Circle(4);
        System.out.println("c1 == c2: " + (crc1 == crc2));

        System.out.println("c1.equals(c2) " + (crc1.equals(crc2)));
    }
}
