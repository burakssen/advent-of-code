import java.util.Scanner;
import java.io.File;
import java.security.*;

public class Main {
    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Usage: java Main <input.txt>");
            System.exit(1);
        }

        String filename = args[0];

        try {
            Scanner scanner = new Scanner(new File(filename));
            while (scanner.hasNextLine()) {
                String prefix = scanner.nextLine();
                int i = 0;
                boolean leading_5_found = false;

                while (true) {
                    String input = prefix + i;
                    byte[] inputBytes = input.getBytes("UTF-8");
                    MessageDigest md = MessageDigest.getInstance("MD5");
                    byte[] hashBytes = md.digest(inputBytes);

                    boolean leading_5 = hashBytes[0] == 0 && hashBytes[1] == 0 && (hashBytes[2] & 0xF0) == 0;
                    boolean leading_6 = leading_5 && (hashBytes[2] & 0xFF) == 0;

                    if (leading_5 && !leading_5_found) {
                        System.out.println("Part 1: " + i);
                        leading_5_found = true;
                    }

                    if (leading_6) {
                        System.out.println("Part 2: " + i);
                        break;
                    }
                    i++;
                }
            }
            scanner.close();
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }
}