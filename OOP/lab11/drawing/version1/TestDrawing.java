package drawing.version1;

import shapes.Circle;
import shapes.Rectangle;
import shapes2d.Square;

public class TestDrawing {

	public static void main(String[] args) {

		Drawing drawing = new Drawing();

		drawing.addCircle(new Circle(5));
		drawing.addRectangle(new Rectangle(5, 6));
		drawing.addSquare(new Square(5));

		System.out.println("Total area = " + drawing.calculateTotalArea());
	}

}