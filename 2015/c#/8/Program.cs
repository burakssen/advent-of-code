using System;
using System.IO;

class Program
{
    static void Main(string[] args)
    {
        if (args.Length < 1)
        {
            Console.Error.WriteLine("Error: missing input file");
            Environment.Exit(1);
        }

        string filename = args[0];

        if (!File.Exists(filename))
        {
            Console.Error.WriteLine("Error: cannot open file");
            Environment.Exit(1);
        }

        int totalCodeChars = 0;
        int totalStringChars = 0;

        int totalOriginalChars = 0;
        int totalEncodedChars = 0;

        using (StreamReader file = new StreamReader(filename))
        {
            string line;
            while ((line = file.ReadLine()) != null)
            {
                // Remove newline character if present
                if (line.Length > 0 && line[line.Length - 1] == '\n')
                {
                    line = line.Substring(0, line.Length - 1);
                }

                // Part 1: Count characters
                int codeChars = 0;
                int stringChars = 0;
                CountCharacters(line, ref codeChars, ref stringChars);

                totalCodeChars += codeChars;
                totalStringChars += stringChars;

                // Part 2: Encode string
                int originalLen = line.Length;
                totalOriginalChars += originalLen;

                string encoded = EncodeString(line);
                int encodedLen = encoded.Length;
                totalEncodedChars += encodedLen;
            }
        }

        Console.WriteLine("Part 1: " + (totalCodeChars - totalStringChars));
        Console.WriteLine("Part 2: " + (totalEncodedChars - totalOriginalChars));
    }

    static void CountCharacters(string input, ref int codeChars, ref int stringChars)
    {
        int len = input.Length;
        codeChars += len;

        for (int i = 1; i < len - 1; ++i)
        {
            if (input[i] == '\\')
            {
                if (i + 1 < len - 1)
                {
                    switch (input[i + 1])
                    {
                        case '\\':
                        case '"':
                            stringChars++;
                            i++;
                            break;
                        case 'x':
                            if (i + 3 < len - 1)
                            {
                                stringChars++;
                                i += 3;
                            }
                            break;
                        default:
                            stringChars++;
                            break;
                    }
                }
            }
            else
            {
                stringChars++;
            }
        }
    }

    static string EncodeString(string input)
    {
        string output = "\"";

        foreach (char ch in input)
        {
            if (ch == '"' || ch == '\\')
            {
                output += '\\';
            }
            output += ch;
        }

        output += "\"";
        return output;
    }
}
