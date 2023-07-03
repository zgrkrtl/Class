public class MyDateTime extends Object {
    MyDate date;

    MyTime time;

    public MyDateTime(MyDate date, MyTime time) {
        this.date = date;
        this.time = time;
    }

    public String toString() {
        return date + " " + time;
    }

    public void incrementDay() {
        date.incrementDay();
    }

    public void incrementHour() {
        incrementHour(1);
    }

    public int incrementHour(int diff) {
        int dayDiff = time.incrementHour(diff);

        time.incrementHour(diff);

        if (dayDiff < 0)
            date.decrementDay(-dayDiff);
        else
            date.incrementDay(dayDiff);
        return dayDiff;
    }

    public void decrementHour(int diff) {
        incrementHour(-diff);
    }

    public void incrementMinute(int diff) {
        int dayDiff = time.incrementMinute(diff);
        if (dayDiff < 0)
            date.decrementDay(-dayDiff);
        else
            date.incrementDay(dayDiff);

    }

    public void decrementMinute(int diff) {
        incrementMinute(-diff);
    }

    public void incrementYear(int diff) {
        date.incrementYear(diff);
    }

    public void decrementDay() {
        date.decrementDay();
    }

    public void decrementYear() {
        date.decrementYear();
    }

    public void decrementMonth() {
        date.decrementMonth();
    }

    public void incrementDay(int diff) {
        date.incrementDay(diff);
    }

    public void decrementMonth(int diff) {
        date.decrementMonth(diff);
    }

    public void decrementDay(int diff) {
        date.decrementDay(diff);
    }

    public void incrementMonth(int diff) {
        date.incrementMonth(diff);
    }

    public void decrementYear(int diff) {
        date.decrementYear(diff);
    }

    public void incrementMonth() {
        date.incrementMonth();
    }

    public void incrementYear() {
        date.incrementYear();
    }

    // public boolean isBefore(MyDateTime anotherDateTime) {
    // return isBefore(anotherDateTime);
    // }

    // public boolean isAfter(MyDateTime anotherDateTime) {
    // return isAfter(anotherDateTime);
    // }

    // public String dayTimeDifference(MyDateTime anotherDateTime) {
    // return date.dayTimeDifference(anotherDateTime);
    // }
}
