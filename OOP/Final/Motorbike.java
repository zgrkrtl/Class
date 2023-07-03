public interface Motorbike extends Vehicle {
    void setSeatHeating(boolean seatHeating);
    // Parent Vehicle already determines price and getopts methods
    // but seatHeating is special for motorbikes

}