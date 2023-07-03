import java.io.IOException;
import java.util.Scanner;

public class TicTacToe {

	public static void main(String[] args) throws IOException {
		Scanner reader = new Scanner(System.in);
		char[][] board = { { ' ', ' ', ' ' }, { ' ', ' ', ' ' }, { ' ', ' ', ' ' } };
		int moveCount = 0;
		int currentPlayer = 0;

		printBoard(board);

		while (moveCount < 9) {
			System.out.println("Player " + (currentPlayer + 1) + " enter row number:");
			int row = reader.nextInt();
			System.out.print("Player" + (currentPlayer + 1) + " enter column number:");
			int col = reader.nextInt();
			if (row > 0 && row < 4 && col > 0 && col < 4 && board[row - 1][col - 1] == ' ') {
				board[row - 1][col - 1] = currentPlayer == 0 ? 'X' : 'O';
				moveCount++;

				printBoard(board);
				boolean win = checkboard(board, row - 1, col - 1);
				if (win) {
					System.out.println("Player " + (currentPlayer + 1) + " is the winner");
					break;
				}
				currentPlayer = (currentPlayer + 1) % 2;
			} else {
				System.out.println("It's not a valid move");
			}

		}
		reader.close();
	}

	public static boolean checkboard(char[][] board, int row, int col) {
		char symbol = board[row][col];
		boolean win = true;
		for (int c = 0; c < 3; c++) {
			if (board[row][c] != symbol) {
				win = false;
				break;
			}
		}
		if (win)
			return true;

		win = true;
		for (int r = 0; r < 3; r++) {
			if (board[r][col] != symbol) {
				win = false;
				break;
			}
		}
		if (win)
			return true;

		if (row == col) {
			win = true;
			for (int r = 0, c = 0; r < 3; r++, c++) {
				if (board[r][c] != symbol) {
					win = false;
					break;
				}
			}
		}
		if (win)
			return true;

		return false;
	}

	public static void printBoard(char[][] board) {
		System.out.println("    1   2   3");
		System.out.println("   -----------");
		for (int row = 0; row < 3; ++row) {
			System.out.print(row + 1 + " ");
			for (int col = 0; col < 3; ++col) {
				System.out.print("|");
				System.out.print(" " + board[row][col] + " ");
				if (col == 2)
					System.out.print("|");

			}
			System.out.println();
			System.out.println("   -----------");

		}

	}

}