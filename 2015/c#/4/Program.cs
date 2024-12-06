using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;

namespace main
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length != 1)
            {
                Console.WriteLine("Usage: dotnet run <input.txt>");
                return;
            }

            string filename = args[0];

            if (!File.Exists(filename))
            {
                Console.WriteLine($"File {filename} does not exist.");
                return;
            }

            string prefix;
            using (StreamReader sr = new StreamReader(filename))
            {
                prefix = sr.ReadLine();
            }

            if (prefix == null || prefix.Length < 8)
            {
                Console.WriteLine("Invalid prefix in file.");
                return;
            }

            prefix = prefix.Substring(0, 8);

            int i = 0;
            bool found_5_leading = false;

            using (MD5 md5 = MD5.Create())
            {
                while (true)
                {
                    string input = prefix + i.ToString();
                    byte[] inputBytes = Encoding.ASCII.GetBytes(input);
                    byte[] hashBytes = md5.ComputeHash(inputBytes);

                    // the lowest positive number that produces a hash with 6 leading zeros
                    if (hashBytes[0] == 0 && hashBytes[1] == 0 && hashBytes[2] == 0)
                    {
                        Console.WriteLine($"Part 2: {i}");
                        break;
                    }

                    if (hashBytes[0] == 0 && hashBytes[1] == 0 && (hashBytes[2] & 0xF0) == 0 && !found_5_leading)
                    {
                        found_5_leading = true;
                        Console.WriteLine($"Part 1: {i}");
                    }

                    i++;
                }
            }
        }
    }
}
