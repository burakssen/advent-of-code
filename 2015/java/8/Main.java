import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {
    // countChars calculates the number of characters in the code and the
    // number of characters in the string representation, excluding escape
    // sequences.
    public static void countChars(String s, Counter codeChars, Counter stringChars) {
        codeChars.value += s.length();

        for (int i = 1; i < s.length() - 1; i++) { // Skip first and last quote
            if (s.charAt(i) == '\\') {
                if (i + 1 < s.length() - 1) {
                    switch (s.charAt(i + 1)) {
                        case '\\':
                        case '"':
                            stringChars.value++;
                            i++;
                            break;
                        case 'x':
                            if (i + 3 < s.length() - 1) {
                                stringChars.value++;
                                i += 3;
                            }
                            break;
                        default:
                            stringChars.value++;
                    }
                }
            } else {
                stringChars.value++;
            }
        }
    }

    // encodeString returns an encoded version of the input string and its length.
    public static EncodedString encodeString(String s) {
        StringBuilder b = new StringBuilder();
        b.append("\"");

        for (char ch : s.toCharArray()) {
            if (ch == '"' || ch == '\\') {
                b.append('\\');
            }
            b.append(ch);
        }

        b.append("\"");
        return new EncodedString(b.toString(), b.length());
    }

    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("missing input file");
            System.exit(1);
        }

        try (BufferedReader reader = new BufferedReader(new FileReader(args[0]))) {
            int totalCodeChars = 0;
            int totalStringChars = 0;
            int totalOrigChars = 0;
            int totalEncChars = 0;

            String line;
            while ((line = reader.readLine()) != null) {
                // Part 1: Count characters
                Counter codeChars = new Counter();
                Counter stringChars = new Counter();
                countChars(line, codeChars, stringChars);
                totalCodeChars += codeChars.value;
                totalStringChars += stringChars.value;

                // Part 2: Encode string
                totalOrigChars += line.length();
                EncodedString encString = encodeString(line);
                totalEncChars += encString.length;
            }

            System.out.printf("Part 1: %d\n", totalCodeChars - totalStringChars);
            System.out.printf("Part 2: %d\n", totalEncChars - totalOrigChars);
        } catch (IOException e) {
            System.err.printf("cannot open file: %v\n", e);
            System.exit(1);
        }
    }
}

class Counter {
    int value = 0;
}

class EncodedString {
    String encoded;
    int length;

    EncodedString(String encoded, int length) {
        this.encoded = encoded;
        this.length = length;
    }
}
