package lab12;

public class StackItem {

    private Object item;

    private StackItem next;

    public StackItem(Object item) {
        this.item = item;
    }

    public Object getItem() {
        return item;
    }

    public StackItem getNext() {
        return next;
    }

    public void setNext(StackItem next) {
        this.next = next;
    }
}