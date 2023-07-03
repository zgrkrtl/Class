package lab9.shapes2d;

public class Square {
    protected final int sd;

    public int area() {
        return sd * sd;
    }

    public Square(int sd) {
        this.sd = sd;
    }

    public String toString() {
        return "Side= " + sd;
    }

    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj.getClass().equals(this.getClass())) {
            Square c = (Square) obj;
            return sd == c.sd;
        } else
            return false;
    }
}