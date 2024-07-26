import java.io.File;
import java.util.Scanner;
import java.util.HashMap;
import java.util.ArrayDeque;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

enum InstructionType {
    AND,
    OR,
    LSHIFT,
    RSHIFT,
    NOT,
    ASSIGN
}

class Instruction {
    public InstructionType type;
    public String op1;
    public String op2;
    public String result;

    public Instruction(InstructionType type, String op1, String op2, String result) {
        this.type = type;
        this.op1 = op1;
        this.op2 = op2;
        this.result = result;
    }

    @Override
    public String toString() {
        return "Instruction [type=" + type + ", op1=" + op1 + ", op2=" + op2 + ", result=" + result + "]";
    }
}

public class Main {

    public static ArrayDeque<Instruction> instructions = new ArrayDeque<Instruction>();
    public static HashMap<String, Integer> registers = new HashMap<String, Integer>();

    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Usage: java Main <input.txt>");
            System.exit(1);
        }

        String filename = args[0];
        File file = new File(filename);
        if (!file.exists()) {
            System.out.println("File not found: " + filename);
            System.exit(1);
        }

        try {

            Scanner scanner = new Scanner(file);
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                // use pattern matching to parse the line
                // and create an Instruction object
                Instruction instruction = null;
                String op1 = "";
                String op2 = "";
                String result = "";

                if (line.contains("AND") || line.contains("OR") || line.contains("LSHIFT") || line.contains("RSHIFT")) {
                    String regex = "(\\S+)\\s+(:?AND|OR|LSHIFT|RSHIFT)\\s+(\\S+)\\s+->\\s+(\\S+)";
                    Pattern pattern = Pattern.compile(regex);
                    Matcher matcher = pattern.matcher(line);

                    if (matcher.find()) {
                        op1 = matcher.group(1);
                        op2 = matcher.group(3);
                        result = matcher.group(4);
                    }

                    if (line.contains("AND")) {
                        instruction = new Instruction(InstructionType.AND, op1, op2, result);
                    }

                    if (line.contains("OR")) {
                        instruction = new Instruction(InstructionType.OR, op1, op2, result);
                    }

                    if (line.contains("LSHIFT")) {
                        instruction = new Instruction(InstructionType.LSHIFT, op1, op2, result);
                    }

                    if (line.contains("RSHIFT")) {
                        instruction = new Instruction(InstructionType.RSHIFT, op1, op2, result);

                    }
                    instructions.add(instruction);

                }

                if (line.contains("NOT")) {
                    String regex = "NOT\\s+(\\S+)\\s+->\\s+(\\S+)";
                    Pattern pattern = Pattern.compile(regex);
                    Matcher matcher = pattern.matcher(line);

                    if (matcher.find()) {
                        op1 = matcher.group(1);
                        result = matcher.group(2);
                    }

                    instruction = new Instruction(InstructionType.NOT, op1, "", result);
                    instructions.add(instruction);

                }

                if (instruction == null) {
                    String regex = "(\\S+)\\s+->\\s+(\\S+)";
                    Pattern pattern = Pattern.compile(regex);
                    Matcher matcher = pattern.matcher(line);

                    if (matcher.find()) {
                        op1 = matcher.group(1);
                        result = matcher.group(2);
                    }

                    instruction = new Instruction(InstructionType.ASSIGN, op1, "", result);
                    instructions.add(instruction);
                }

            }

            // Instruction a = instructions.stream().filter(i ->
            // i.result.equals("a")).findFirst().get();

            Integer a_result = eval("a");
            System.out.println("Part 1: " + a_result);

            registers.clear();
            registers.put("b", a_result);
            Integer a_result_part2 = eval("a");
            System.out.println("Part 2: " + a_result_part2);

            scanner.close();
        } catch (Exception e) {
            System.out.println(e);
            System.out.println("Error reading file: " + filename);
            System.exit(1);
        }
    }

    public static Integer eval(String target) {
        if (registers.containsKey(target)) {
            return registers.get(target);
        }

        Instruction instruction = null;

        for (Instruction i : instructions) {
            if (i.result.equals(target)) {
                instruction = i;
                break;
            }
        }

        Integer op1 = instruction.op1.matches("\\d+") ? Integer.parseInt(instruction.op1) : eval(instruction.op1);

        Integer op2 = 0;
        if (instruction.op2.length() > 0) {
            op2 = instruction.op2.matches("\\d+") ? Integer.parseInt(instruction.op2) : eval(instruction.op2);
        }

        Integer result = 0;
        switch (instruction.type) {
            case AND:
                result = op1 & op2;
                break;
            case OR:
                result = op1 | op2;
                break;
            case LSHIFT:
                result = op1 << op2;
                break;
            case RSHIFT:
                result = op1 >> op2;
                break;
            case NOT:
                result = ~op1;
                break;
            case ASSIGN:
                result = op1;
                break;
        }

        registers.put(target, result);
        return result;
    }
}