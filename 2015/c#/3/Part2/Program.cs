using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    struct Santa
    {
        public int X { get; private set; }
        public int Y { get; private set; }

        public Santa(int x, int y)
        {
            X = x;
            Y = y;
        }

        public void Move(char direction)
        {
            switch (direction)
            {
                case '^':
                    Y++;
                    break;
                case 'v':
                    Y--;
                    break;
                case '>':
                    X++;
                    break;
                case '<':
                    X--;
                    break;
            }
        }

        public (int, int) GetPosition() => (X, Y);
    }

    static void Main(string[] args)
    {
        if (args.Length < 1)
        {
            Console.WriteLine("Usage: dotnet run <input_file>");
            Environment.Exit(1);
        }

        string filename = args[0];

        if (!File.Exists(filename))
        {
            Console.WriteLine($"Error: Could not open file {filename}");
            Environment.Exit(1);
        }

        try
        {
            var directions = File.ReadAllText(filename);

            var santa = new Santa(0, 0);
            var roboSanta = new Santa(0, 0);
            var visitedPositions = new HashSet<(int, int)>
            {
                santa.GetPosition(),
                roboSanta.GetPosition()
            };

            bool santaTurn = true;

            foreach (char direction in directions)
            {
                var currentSanta = santaTurn ? santa : roboSanta;
                currentSanta.Move(direction);

                visitedPositions.Add(currentSanta.GetPosition());

                if (santaTurn)
                    santa = currentSanta;
                else
                    roboSanta = currentSanta;

                santaTurn = !santaTurn;
            }

            int count = visitedPositions.Count;
            Console.WriteLine($"Part 2: {count}");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
            Environment.Exit(1);
        }
    }
}
