import java.io.File;
import java.util.Arrays;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Usage: java Main <input_file>");
            return;
        }

        String input = args[0];

        File file = new File(input);
        if (!file.exists()) {
            System.out.println("File not found: " + input);
            return;
        }

        try {
            int paper_size = 0;
            int ribbon_lenght = 0;
            Scanner scanner = new Scanner(file);
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                // split line by x
                String[] parts = line.split("x");
                if (parts.length != 3) {
                    System.out.println("Invalid input: " + line);
                    continue;
                }

                int l = Integer.parseInt(parts[0]);
                int w = Integer.parseInt(parts[1]);
                int h = Integer.parseInt(parts[2]);

                int[] sides = new int[] { l, w, h };
                Arrays.sort(sides);

                int side1 = sides[0];
                int side2 = sides[1];

                // Ribbon
                ribbon_lenght += 2 * side1 + 2 * side2 + l * w * h;

                int area = 2 * l * w + 2 * w * h + 2 * h * l;
                int slack = Math.min(Math.min(l * w, w * h), h * l);
                paper_size += area + slack;
            }

            scanner.close();

            System.out.println("Part 1: " + paper_size);
            System.out.println("Part 2: " + ribbon_lenght);
        } catch (Exception e) {
            System.out.println("Error reading file: " + e.getMessage());
        }
    }
}