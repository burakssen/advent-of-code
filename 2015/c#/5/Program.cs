using System;
using System.IO;

namespace main
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length < 1)
            {
                Console.WriteLine("Usage: dotnet run <input.txt>");
                return;
            }

            string filename = args[0];

            if (!File.Exists(filename))
            {
                Console.WriteLine($"Error: Could not open file {filename}");
                return;
            }

            int part1_count = 0;
            int part2_count = 0;

            foreach (var line in File.ReadLines(filename))
            {
                string trimmedLine = line.Trim();

                if (CheckGoodStringPart1(trimmedLine))
                    part1_count++;

                if (CheckGoodStringPart2(trimmedLine))
                    part2_count++;
            }

            Console.WriteLine($"Part 1: {part1_count}");
            Console.WriteLine($"Part 2: {part2_count}");
        }

        static bool CheckGoodStringPart1(string s)
        {
            if (s.Length < 3)
                return false;

            if (s.Contains("ab") || s.Contains("cd") || s.Contains("pq") || s.Contains("xy"))
                return false;

            int vowels = 0;
            for (int i = 0; i < s.Length; i++)
            {
                if (s[i] == 'a' || s[i] == 'e' || s[i] == 'i' || s[i] == 'o' || s[i] == 'u')
                    vowels++;

                if (vowels >= 3)
                    break;
            }

            if (vowels < 3)
                return false;

            for (int i = 0; i < s.Length - 1; i++)
            {
                if (s[i] == s[i + 1])
                    return true;
            }

            return false;
        }

        static bool CheckGoodStringPart2(string s)
        {
            if (s.Length < 3)
                return false;

            bool foundPair = false;
            for (int i = 0; i < s.Length - 1; i++)
            {
                string pair = s.Substring(i, 2);
                if (s.IndexOf(pair, i + 2) != -1)
                {
                    foundPair = true;
                    break;
                }
            }

            if (!foundPair)
                return false;

            for (int i = 0; i < s.Length - 2; i++)
            {
                if (s[i] == s[i + 2])
                    return true;
            }

            return false;
        }
    }
}
