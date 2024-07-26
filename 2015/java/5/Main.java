import java.io.File;
import java.util.Scanner;

public class Main {

    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Usage: java Main <input.txt>");
            System.exit(1);
        }

        String filename = args[0];

        try {
            Scanner scanner = new Scanner(new File(filename));
            int part1_count = 0, part2_count = 0;
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();

                if (check_good_string_part1(line)) {
                    part1_count++;
                }

                if (check_good_string_part2(line)) {
                    part2_count++;
                }
            }

            System.out.println("Part 1: " + part1_count);
            System.out.println("Part 2: " + part2_count);
            scanner.close();
        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }
    }

    public static boolean check_good_string_part1(String s) {
        int vowels = 0;
        boolean double_letter = false;
        boolean bad_string = false;

        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u') {
                vowels++;
            }

            if (i < s.length() - 1 && s.charAt(i + 1) == c) {
                double_letter = true;
            }

            if (i < s.length() - 1 && (c == 'a' && s.charAt(i + 1) == 'b' || c == 'c' && s.charAt(i + 1) == 'd'
                    || c == 'p' && s.charAt(i + 1) == 'q' || c == 'x' && s.charAt(i + 1) == 'y')) {
                bad_string = true;
            }
        }

        return vowels >= 3 && double_letter && !bad_string;
    }

    public static boolean check_good_string_part2(String s) {
        boolean pair = false;
        boolean repeat = false;

        for (int i = 0; i < s.length() - 1; i++) {
            String pair_str = s.substring(i, i + 2);
            if (s.indexOf(pair_str, i + 2) != -1) {
                repeat = true;
            }

            if (i > 0 && s.charAt(i - 1) == s.charAt(i + 1)) {
                pair = true;
            }
        }

        return pair && repeat;
    }
}