//This interface defines a contract for classes that represent vehicles. 
//This interface also defines a set of methods that a class must implement if it implements that interface. 
//It is used to specify a behavior or functionality that multiple classes can share.
public interface Vehicle {
    double getPrice(); // This method returns the price of the vehicle.

    String getOptions(); // This method returns a string representation of the optional features selected
                         // for the vehicle.

    void setAbs(boolean abs); // setAbs is consistent for all vehicle
}
