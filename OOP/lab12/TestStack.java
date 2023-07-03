package lab12;

public class TestStack {
    public static void main(String[] args) {
        testStack(new Stackimpt());

        testStack(new StackArrayListImpt());

    }

    public static void testStack(Stack stack) {
        stack.push(5);
        stack.push(6);
        stack.push(2);
        stack.push(11);
        stack.push(23);

        while (!stack.empty()) {
            System.out.println(stack.pop());
        }
    }

}
