package drawing.version2;

import java.util.ArrayList;
import java.util.concurrent.Callable;

import shapes.Circle;
import shapes.Rectangle;
import shapes.Square;

public class Drawing {

	private ArrayList<Object> shapes = new ArrayList<>();

	public double calculateTotalArea() {
		double totalArea = 0;

		for (Object shape : shapes) {
			if (shape instanceof Circle) {
				Circle c = (Circle) shape;
				totalArea += c.area();
			}
			if (shape instanceof Rectangle) {
				Rectangle r = (Rectangle) shape;
				totalArea += r.area();
			}
			if (shape instanceof Square) {
				Square s = (Square) shape;
				totalArea += s.area();
			}

		}
		return totalArea;
	}

	public void addShape(Object shape) {
		shapes.add(shape);
	}

}
