using System;

namespace main
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length < 1)
            {
                Console.WriteLine("Usage: dotnet run <input>");
                return;
            }

            string filename = args[0];
            string[] lines = System.IO.File.ReadAllLines(filename);

            int floor = 0;
            int basement = 0;
            int count = 0;
            // Read each character in the file
            foreach (string line in lines)
            {
                foreach (char c in line)
                {
                    count++;
                    if (c == '(')
                    {
                        floor++;
                    }
                    else if (c == ')')
                    {
                        floor--;
                    }

                    if (floor == -1 && basement == 0)
                    {
                        basement = count;
                    }
                }
            }

            Console.WriteLine("Part 1: " + floor);
            Console.WriteLine("Part 2: " + basement);
        }
    }
}