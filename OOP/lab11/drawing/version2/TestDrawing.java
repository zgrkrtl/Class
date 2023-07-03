package drawing.version2;

import shapes.Circle;
import shapes.Rectangle;

public class TestDrawing {

	public static void main(String[] args) {

		Drawing drawing = new Drawing();

		drawing.addCircle(new Circle(5));
		drawing.addRectangle(new Rectangle(5, 6));

		System.out.println("Total area = " + drawing.calculateTotalArea());
	}

}
