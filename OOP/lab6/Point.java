package lab6;

public class Point {
    private int xCoord;
    private int yCoord;

    public Point(int xCoord, int yCoord) {
        this.xCoord = xCoord;
        this.yCoord = yCoord;
    }

    public int getxCoord() {
        return xCoord;
    }

    public void setxCoord(int xCoord) {
        this.xCoord = xCoord;
    }

    public int getyCoord() {
        return yCoord;
    }

    public void setyCoord(int yCoord) {
        this.yCoord = yCoord;
    }

    public double distanceFromPoint(Point point) {
        int xDiff = xCoord - point.xCoord;
        int yDiff = yCoord - point.yCoord;
        return Math.sqrt(xDiff * xDiff + yDiff * yDiff);
    }
}
