import java.io.File;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        // Get command line arguments
        String[] arguments = args;

        if (arguments.length < 1) {
            System.out.println("Usage: java Main <input_file>");
            return;
        }

        // open file
        String filename = arguments[0];
        File file = new File(filename);
        try {
            Scanner scanner = new Scanner(file);

            var floor = 0;
            var count = 0;
            var basement = 0;
            // read file
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                for (int i = 0; i < line.length(); i++) {
                    if (line.charAt(i) == '(') {
                        floor++;
                    } else if (line.charAt(i) == ')') {
                        floor--;
                    }

                    count++;
                    if (floor == -1 && basement == 0) {
                        basement = count;
                    }
                }
            }

            System.out.println("Part 1: " + floor);
            System.out.println("Part 2: " + basement);

        } catch (Exception e) {
            System.out.println("Error: " + e);
        }

    }
}