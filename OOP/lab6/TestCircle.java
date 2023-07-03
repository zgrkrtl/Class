package lab6;

public class TestCircle {

    public static void main(String[] args) {
        Circle crc = new Circle(8, new Point(5, 14));
        System.out.println("Area of circle " + crc.area());
        System.out.printf("Perimeter of circle " + crc.perimeter());

        Circle crc2 = new Circle(3, new Point(11, 2));

        if (crc.intersect(crc2)) {
            System.out.printf("intersected");
        } else
            System.out.printf(" Not intersected");
    }
}