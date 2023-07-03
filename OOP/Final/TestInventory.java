public class TestInventory {
    public static void main(String[] args) {
        Inventory inventory = new Inventory();
        Car car = new Sivic(); // 50000
        car.setAbs(true); // 5000
        car.setMusicSystem(true); // 1000
        car.setAirBag(true); // 3000
        inventory.add(car);
        car = new Sivic(); // 50000
        car.setAbs(true); // 5000
        car.setSunRoof(true); // 2000
        inventory.add(car);
        car = new Sity(); // 40000
        car.setMusicSystem(true); // 1000
        car.setSunRoof(true); // 2000
        inventory.add(car);
        Motorbike mBike = new Racer(); // 60000
        mBike.setAbs(true); // 5000
        mBike.setSeatHeating(true);// 2000
        inventory.add(mBike);
        mBike = new Scooter();// 20000
        mBike.setSeatHeating(true);// 2000
        inventory.add(mBike);
        // print the inventory
        System.out.println(inventory);
        // Expected output :
        // Sivic with ABS, Music System, Air Bag optional having a total price of 59000
        // TL

        // Sivic with ABS, Sunroof having a total price of 57000 TL
        // Sity with Music System, Sunroof having a total price of 43000 TL

        // Racer with ABS, Seat Heating having a total price of 67000 TL
        // Scooter with Seat Heating having a total price of 22000 TL
        // TOTAL : 5 Vehicles including 3 Cars and 2 Motorbikes having a total price of
        // 248000 TL

    }
}