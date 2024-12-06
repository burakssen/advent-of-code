using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main(string[] args)
    {
        if (args.Length < 1)
        {
            Console.WriteLine($"Usage: {AppDomain.CurrentDomain.FriendlyName} <input.txt>");
            return;
        }

        string filePath = args[0];

        if (!File.Exists(filePath))
        {
            Console.WriteLine($"Error: Unable to open file {filePath}");
            return;
        }

        bool[,] lights = new bool[1000, 1000];

        foreach (string line in File.ReadLines(filePath))
        {
            var (action, startx, starty, endx, endy) = ParseLine(line);

            for (int i = startx; i <= endx; i++)
            {
                for (int j = starty; j <= endy; j++)
                {
                    lights[i, j] = action switch
                    {
                        "on" => true,
                        "off" => false,
                        _ => !lights[i, j]
                    };
                }
            }
        }

        int count = lights.Cast<bool>().Count(b => b);

        Console.WriteLine($"Part 1: {count}");
    }

    static (string action, int startx, int starty, int endx, int endy) ParseLine(string line)
    {
        string[] parts = line.Split(' ');

        if (parts[0] == "turn")
        {
            string action = parts[1];
            var (startx, starty) = ParseCoordinates(parts[2]);
            var (endx, endy) = ParseCoordinates(parts[4]);
            return (action, startx, starty, endx, endy);
        }
        else
        {
            string action = parts[0];
            var (startx, starty) = ParseCoordinates(parts[1]);
            var (endx, endy) = ParseCoordinates(parts[3]);
            return (action, startx, starty, endx, endy);
        }
    }

    static (int x, int y) ParseCoordinates(string coordinates)
    {
        var coords = coordinates.Split(',').Select(int.Parse).ToArray();
        return (coords[0], coords[1]);
    }
}
