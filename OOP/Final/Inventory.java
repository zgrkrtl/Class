import java.util.ArrayList;

//210709604
public class Inventory {
    // this field is for the vehicles in the inventory. It is initialized as an ->
    // empty.
    // ArrayList(type of Vehicle) in the constructor.
    private ArrayList<Vehicle> vehicles;

    // Constructor
    public Inventory() {
        vehicles = new ArrayList<>();
    }

    // this method allows adding a vehicle to the inventory by appending it to the
    // vehicles Array list.
    public void add(Vehicle vehicle) {
        vehicles.add(vehicle);
    }

    public String toString() {
        String str = "";
        // The totalPrice, carCount, and motorbikeCount variables are initialized to
        // zero. These variables will be used to calculate the total price and count of
        // cars and motorbikes in the inventory.
        double Total = 0;
        int carCount = 0;
        int motorbikeCount = 0;
        // List of vehicles in for loop
        for (Vehicle vehicle : vehicles) {
            // String concatenations to build up result
            str += vehicle.getClass().getSimpleName(); // gets the name of vehicle by class
            str += " with ";
            str += vehicle.getOptions();
            str += " having a total price of ";
            double price = vehicle.getPrice();
            str += Double.toString(price);
            str += " TL\n";

            Total += vehicle.getPrice();
            // this if-else statement checks if the vehicle is instance of car or motorbike
            if (vehicle instanceof Car) {
                carCount++;
            } else if (vehicle instanceof Motorbike) {
                motorbikeCount++;
            }
        }
        // bring together all the data finally and concat again
        str += "TOTAL: ";
        str += "Vehicles including ";
        str += carCount;
        str += " Cars and ";
        str += motorbikeCount;
        str += " Motorbikes having a total price of ";
        str += Total;
        str += " TL";

        return str;
    }
}