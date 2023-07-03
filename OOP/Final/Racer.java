import java.util.ArrayList;

public class Racer implements Motorbike {
    // Fields
    private boolean abs;
    private boolean seatHeating;

    // This method returns the price of the vehicle. With conditions of extra
    // situations
    public int getPrice() {
        int totalPrice = 60000;
        if (abs) {
            totalPrice += 5000;
        }
        if (seatHeating) {
            totalPrice += 2000;
        }
        return totalPrice;
    }

    // This method below returns a string representation of the
    // optional features selected
    // for the racer.
    public String getOptions() {
        ArrayList<String> opts = new ArrayList<>();
        if (abs) {
            opts.add("ABS ");
        }
        if (seatHeating) {
            opts.add("Seat Heating ");
        }

        String str = "";
        for (int i = 0; i < opts.size(); i++) {
            str += opts.get(i);
            if (opts.get(i) != opts.get(opts.size() - 1))
                str += ",";
        }
        return str;
    }

    // These two methods below is set methods for racer's extra units
    // with boolean expressions if it has it or not
    public void setAbs(boolean abs) {
        this.abs = abs;
    }

    public void setSeatHeating(boolean seatHeating) {
        this.seatHeating = seatHeating;
    }
}
