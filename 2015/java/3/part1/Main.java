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
                    Position newPos = new Position(pos.x, pos.y);
                    char direction = line.charAt(i);
                    newPos.move(direction);
                    newPos.reHash();
                    map.put(newPos, map.getOrDefault(newPos, 0) + 1);
                    pos = newPos;
                }

                int count = map.size();
                System.out.println("Part 1: " + count);
            }

            scanner.close();

        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }
    }
}