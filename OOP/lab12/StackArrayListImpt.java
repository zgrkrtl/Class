package lab12;

import java.util.ArrayList;
import java.util.List;

public class StackArrayListImpt implements Stack {
    private List<Object> stack = new ArrayList<>();

    @Override
    public void push(Object item) {
        stack.add(item);

    }

    @Override
    public Object pop() {
        if (stack.size() > 0)
            return stack.remove(stack.size() - 1);
        return null;
    }

    @Override
    public boolean empty() {
        return stack.size() == 0;
    }
}
