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

            int paper_size = 0;
            int ribbon_length = 0;
            // Read each character in the file
            foreach (string line in lines)
            {
                string[] dimensions = line.Split('x');
                int a = int.Parse(dimensions[0]);
                int b = int.Parse(dimensions[1]);
                int c = int.Parse(dimensions[2]);

                int area = 2 * a * b + 2 * b * c + 2 * c * a;
                int smallest = Math.Min(a * b, Math.Min(b * c, c * a));
                paper_size += area + smallest;

                int perimeter = 2 * a + 2 * b;
                int volume = a * b * c;
                int smallest_perimeter = Math.Min(perimeter, Math.Min(2 * a + 2 * c, 2 * b + 2 * c));
                ribbon_length += smallest_perimeter + volume;
            }

            Console.WriteLine("Part 1: " + paper_size);
            Console.WriteLine("Part 2: " + ribbon_length);
        }
    }
}