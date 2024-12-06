package Part2;

import java.util.Scanner;
import java.io.File;

import ActionWrapper.ActionWrapper;

public class Main {
    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Usage: java Main <input.txt>");
            return;
        }

        String filename = args[0];

        int grid[][] = new int[1000][1000];

        try {

            File file = new File(filename);

            if (!file.exists()) {
                System.out.println("File not found: " + filename);
                return;
            }

            Scanner scanner = new Scanner(file);
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                ActionWrapper action = new ActionWrapper();
                action.parse(line);

                for (int i = action.start.x; i <= action.end.x; i++) {
                    for (int j = action.start.y; j <= action.end.y; j++) {
                        if (action.action.equals("on")) {
                            grid[i][j] += 1;
                        } else if (action.action.equals("off")) {
                            grid[i][j] -= 1;
                            if (grid[i][j] < 0) {
                                grid[i][j] = 0;
                            }
                        } else if (action.action.equals("toggle")) {
                            grid[i][j] += 2;
                        }
                    }
                }
            }

            int count = 0;
            for (int i = 0; i < 1000; i++) {
                for (int j = 0; j < 1000; j++) {
                    count += grid[i][j];
                }
            }

            System.out.println("Part 2: " + count);

            scanner.close();
        } catch (java.io.FileNotFoundException e) {
            System.out.println("File not found: " + filename);
        }

    }
}