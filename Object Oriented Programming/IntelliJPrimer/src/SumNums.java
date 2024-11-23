import java.util.Scanner;

/**
 * Example program for the IntelliJ primer. There is no need to understand
 * how it works.
 * The original version is deliberately badly formatted.
 */
public class SumNums {
    /**
     * Read 2 integers and print out their sum.
     */
    public void sumTheNums() {
        int first, second;
        Scanner input = new Scanner(System.in);
        System.out.printf("Please enter the first whole number%n");
        first = input.nextInt();

        System.out.printf("Please enter the second whole number%n");
        second = input.nextInt();

        System.out.printf("  %4d%n+ %4d%n------%n%06d%n======%n", first, second, first + second);
    }

    /**
     * @param args Command-line parameters are ignored
     */
    public static void main(String[] args) {
        SumNums sumThing = new SumNums();
        sumThing.sumTheNums();
    }
}
