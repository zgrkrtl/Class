public class MyTime {
    int hour;
    int minute;

    public MyTime(int hour, int minute) {

    }

    public String toString() {
        return (hour < 10 ? "0" : "") + hour + ":" + (minute < 10 ? "0" : "") + minute;
    }

    public int incrementHour(int diff) {
        int dayDiff = 0;

        dayDiff += (hour + diff) / 24;
        if (hour + diff < 0) {
            dayDiff = -1;
        }
        hour = (hour + diff) % 24;

        if (hour < 0) {
            hour += 24;
        }
        return dayDiff;
    }

    public int incrementMinute(int diff) {
        int hourDiff = 0;
        if (minute + diff < 0)
            hourDiff = -1;
        minute = (minute + diff) % 60;
        if (minute < 0)
            minute += 60;

        return incrementHour(hourDiff);
    }
}
