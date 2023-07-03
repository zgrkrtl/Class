package drawing.version3;

import java.util.ArrayList;

import shapes.Shape;

public class Drawing {

	private ArrayList<Shape> shapes = new ArrayList<>();

	public double calculateTotalArea() {
		double totalArea = 0;
		for (Shape shape : shapes) {
			totalArea += shape.area();
		}

		return totalArea;
	}

	public void addShape(Shape shape) {
		shapes.add(shape);
	}

}
