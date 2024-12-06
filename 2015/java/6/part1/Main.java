package Part1;

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

        boolean grid[][] = new boolean[1000][1000];

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
                            grid[i][j] = true;
                        } else if (action.action.equals("off")) {
                            grid[i][j] = false;
                        } else if (action.action.equals("toggle")) {
                            grid[i][j] = !grid[i][j];
                        }
                    }
                }
            }

            int count = 0;
            for (int i = 0; i < 1000; i++) {
                for (int j = 0; j < 1000; j++) {
                    if (grid[i][j]) {
                        count++;
                    }
                }
            }

            System.out.println("Part 1: " + count);

            scanner.close();
        } catch (java.io.FileNotFoundException e) {
            System.out.println("File not found: " + filename);
        }

    }
}