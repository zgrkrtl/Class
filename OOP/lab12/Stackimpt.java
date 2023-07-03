package lab12;

public class Stackimpt implements Stack {

    StackItem top;

    @Override
    public void push(Object item) {
        StackItem stackItem = new StackItem(item);
        stackItem.setNext(top);
        top = stackItem;
    }

    @Override
    public Object pop() {
        if (top != null) {

            Object item = top.getItem();
            top = top.getNext();
            return item;
        } else
            return null;
    }

    @Override
    public boolean empty() {
        return top == null;
    }
}