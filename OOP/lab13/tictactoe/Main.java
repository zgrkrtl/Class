package tictactoe;

import java.util.InputMismatchException;
import java.util.Scanner;
import java.util.concurrent.TransferQueue;

public class Main {

    public static void main(String[] args) throws InvalidMoveException {
        Scanner reader = new Scanner(System.in);

        Board board = new Board();

        System.out.println(board);
        while (!board.isEnded()) {

            int player = board.getCurrentPlayer();

            boolean invalidRow = false;
            int row = 0, col = 0;
            do {
                System.out.print("Player " + player + " enter row number:");
                try {
                    row = Integer.valueOf(reader.nextLine());
                    System.out.println("Valid Integer");
                    invalidRow = false;
                } catch (NumberFormatException ex) {
                    System.out.println("Invalid Integer");
                    invalidRow = true;
                }
            } while (invalidRow);

            boolean invalidColumn = false;
            do {
                System.out.print("Player " + player + " enter column number:");
                try {
                    col = Integer.valueOf(reader.nextLine());
                    System.out.println("Valid Integer");
                    invalidColumn = false;
                } catch (NumberFormatException ex) {
                    System.out.println("Inalid Integer");
                    invalidColumn = true;
                }
            } while (invalidColumn);

            try {
                board.move(row, col);
            } catch (InvalidMoveException e) {
                System.out.println(e.getMessage());
            }
            System.out.println(board);

        }

        reader.close();
    }
}