public class FindPrimes {
    public static void main(String[] args) {
        int number = Integer.parseInt(args[0]);

        for (int i = 2; i < number; i++) {

            int div = 2;

            boolean isPrime = true;

            while (div < i && isPrime) {

                if (i % div == 0)
                    isPrime = false;

                div++;
            }
            if (isPrime)
                System.out.print(i + ",");

        }

    }

}
