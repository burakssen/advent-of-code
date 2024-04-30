import java.io.File;
import java.util.Scanner;
import java.util.Map;
import java.util.HashMap;
import java.util.Objects;

class Position {
    private final int x;
    private final int y;
    private int hashCode;

    public Position(int x, int y) {
        this.x = x;
        this.y = y;
        this.hashCode = Objects.hash(x, y);
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }

        Position pos = (Position) obj;
        return x == pos.x && y == pos.y;
    }

    @Override
    public int hashCode() {
        return hashCode;
    }
}

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
                            pos = new Position(pos.getX(), pos.getY() + 1);
                            break;
                        case '<':
                            pos = new Position(pos.getX() - 1, pos.getY());
                            break;
                        case '>':
                            pos = new Position(pos.getX() + 1, pos.getY());
                            break;
                        case 'v':
                            pos = new Position(pos.getX(), pos.getY() - 1);
                            break;
                        default:
                            System.out.println("Invalid character: " + line.charAt(i));
                            break;
                    }

                    if (map.containsKey(pos)) {
                        map.put(pos, map.get(pos) + 1);
                    } else {
                        map.put(pos, 1);
                    }
                }

                int count = 0;
                for (Map.Entry<Position, Integer> entry : map.entrySet()) {
                    if (entry.getValue() > 0) {
                        count++;
                    }
                }

                System.out.println(count);

            }

            scanner.close();

        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }
    }
}