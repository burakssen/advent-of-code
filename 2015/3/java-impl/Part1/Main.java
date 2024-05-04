package Part1;

import java.io.File;
import java.util.Scanner;
import java.util.Map;
import java.util.HashMap;

import Position.Position;

public class Main {

    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Usage: java Main <input_file>");
            System.exit(1);
        }

        String input = args[0];

        try {
            File file = new File(input);
            Scanner scanner = new Scanner(file);

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();

                if (line.length() <= 0) {
                    continue;
                }

                Position pos = new Position(0, 0);

                Map<Position, Integer> map = new HashMap<>();
                map.put(pos, 1);

                for (int i = 0; i < line.length(); i++) {
                    switch (line.charAt(i)) {
                        case '^':
                            pos = new Position(pos.x, pos.y + 1);
                            break;
                        case '<':
                            pos = new Position(pos.x - 1, pos.y);
                            break;
                        case '>':
                            pos = new Position(pos.x + 1, pos.y);
                            break;
                        case 'v':
                            pos = new Position(pos.x, pos.y - 1);
                            break;
                        default:
                            System.out.println("Invalid character: " + line.charAt(i));
                            break;
                    }

                    map.put(pos, map.getOrDefault(pos, 0) + 1);
                }

                int count = map.size();
                System.out.println(count);
            }

            scanner.close();

        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }
    }
}