import java.util.ArrayList;

public class Car implements Vehicle {
    // Fields
    private double Price;
    private boolean abs = false;
    private boolean musicSystem = false;
    private boolean airBag = false;
    private boolean sunRoof = false;

    // Constructor
    public Car(double Price) {
        this.Price = Price * 1.5;
    }

    // These four methods below is set methods for car's extra units
    // with boolean expressions if it has it or not
    public void setAbs(boolean abs) {
        this.abs = abs;
    }

    public void setMusicSystem(boolean musicSystem) {
        this.musicSystem = musicSystem;
    }

    public void setAirBag(boolean airBag) {
        this.airBag = airBag;
    }

    public void setSunRoof(boolean sunRoof) {
        this.sunRoof = sunRoof;
    }

    // This method returns the price of the vehicle. With conditions of extra
    // situations
    public double getPrice() {
        double newPrice = Price * 1.5;
        if (abs) {
            newPrice += 5000 * 1.2;
        }
        if (musicSystem) {
            newPrice += 1000 * 1.2;
        }
        if (airBag) {
            newPrice += 3000 * 1.2;
        }
        if (sunRoof) {
            newPrice += 2000 * 1.2;
        }
        return newPrice;
    }

    // This method below returns a string representation of the
    // optional features selected
    // for the car and Stores them in a String
    public String getOptions() {
        ArrayList<String> opts = new ArrayList<>();
        if (abs) {
            opts.add("ABS ");
        }
        if (musicSystem) {
            opts.add("Music System ");
        }
        if (airBag) {
            opts.add("Air Bag ");
        }
        if (sunRoof) {
            opts.add("Sunroof ");
        }
        String str = "";
        for (int i = 0; i < opts.size(); i++) {
            str += opts.get(i);
            if (opts.get(i) != opts.get(opts.size() - 1))
                str += ",";
        }
        return str;
    }
}