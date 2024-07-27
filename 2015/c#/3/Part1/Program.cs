using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
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

            (int x, int y) position = (0, 0);
            var grid = new Dictionary<(int, int), int>
            {
                [(position.x, position.y)] = 1
            };

            foreach (char direction in directions)
            {
                switch (direction)
                {
                    case '^':
                        position.y++;
                        break;
                    case 'v':
                        position.y--;
                        break;
                    case '>':
                        position.x++;
                        break;
                    case '<':
                        position.x--;
                        break;
                    default:
                        continue;
                }

                if (grid.ContainsKey(position))
                {
                    grid[position]++;
                }
                else
                {
                    grid[position] = 1;
                }
            }

            int count = grid.Count(kvp => kvp.Value > 0);

            Console.WriteLine($"Part 1: {count}");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
            Environment.Exit(1);
        }
    }
}
