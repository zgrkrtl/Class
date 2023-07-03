package lab6;

public class TestRectangle {

    public static void main(String[] args) {
        Rectangle rc = new Rectangle(new Point(4, 2), 16, 17);
        System.out.println("area of rect " + rc.area());
        System.out.println("perimeter of rect " + rc.perimeter());

        Point[] corners = rc.corners();

        for (int i = 0; i < corners.length; i++) {
            Point p = corners[i];

            if (p == null)
                System.out.println("!");
            else {
                System.out.println("Corner " + (i + 1) + " x = " + p.getxCoord() + " y = " + p.getyCoord());
            }
        }

    }
}