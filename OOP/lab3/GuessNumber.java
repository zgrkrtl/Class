import java.io.IOException;
import java.util.Random;
import java.util.Scanner;

public class GuessNumber {

	public static void main(String[] args) throws IOException {
		Scanner reader = new Scanner(System.in); // Creates an object to read user input
		Random rand = new Random(); // Creates an object from Random class
		int number = rand.nextInt(100); // generates a number between 0 and 99

		System.out.println("Hi! I'm thinking of a number between 0 and 99.");
		System.out.print("Can you guess it: ");

		int guess = reader.nextInt(); // Read the user input

		// if (number == guess) {
		// System.out.println("Congratulations!");
		// } else {
		// System.out.println("Sorry Number was, " + number);
		// }
		int guessCount = 1;
		while (guess != number && guess != -1) {

			if (guess > number)
				System.out.println("mine is smaller than your guess");
			else if (guess < number)
				System.out.println("mine is greater than your guess");

			System.out.println("Sorry!, Type -1 to quit or guess another: ");
			guess = reader.nextInt();
			guessCount++;
		}

		if (guess == number)
			System.out.println(
					"Congragulations You've Found the answer (" + number + ") after " + guessCount + " attempts");
		if (guess == -1)
			System.out.println("Sorry, the number was " + number);

		reader.close(); // Close the resource before exiting
	}

}