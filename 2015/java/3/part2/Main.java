package Part2;

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

                Position santa = new Position(0, 0);
                Position robo_santa = new Position(0, 0);

                Map<Position, Integer> map = new HashMap<>();
                map.put(santa, 1);
                map.put(robo_santa, map.getOrDefault(robo_santa, 0) + 1);

                boolean santa_turn = true;

                for (int i = 0; i < line.length(); i++) {
                    Position pos = santa_turn ? santa : robo_santa;
                    Position newPos = new Position(pos.x, pos.y);
                    char direction = line.charAt(i);
                    newPos.move(direction);
                    newPos.reHash();
                    map.put(newPos, map.getOrDefault(newPos, 0) + 1);
                    if (santa_turn) {
                        santa = newPos;
                    } else {
                        robo_santa = newPos;
                    }
                    santa_turn = !santa_turn;
                }

                int count = map.size();
                System.out.println("Part 2: " + count);
            }

            scanner.close();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
