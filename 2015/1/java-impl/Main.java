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

            

        } catch (Exception e) {
            System.out.println("Error: " + e);
        }

    }
}