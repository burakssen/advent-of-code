#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define MAX_LINE_LENGTH 1024

bool check_good_string_part1(const char *line);
bool check_good_string_part2(const char *line);

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        fprintf(stderr, "Usage: %s <input.txt>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "r");
    if (!file)
    {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    char line[MAX_LINE_LENGTH];
    int part1_count = 0;
    int part2_count = 0;

    while (fgets(line, sizeof(line), file))
    {
        size_t len = strlen(line);
        if (line[len - 1] == '\n')
            line[len - 1] = '\0'; // Remove newline character

        if (check_good_string_part1(line))
            part1_count++;

        if (check_good_string_part2(line))
            part2_count++;
    }

    printf("Part 1: %d\n", part1_count);
    printf("Part 2: %d\n", part2_count);

    fclose(file);
    return EXIT_SUCCESS;
}

bool check_good_string_part1(const char *line)
{
    int vowels = 0;
    bool double_letter = false;
    bool bad_string = false;

    for (int i = 0; line[i]; i++)
    {
        if (strchr("aeiou", line[i]))
            vowels++;

        if (i > 0 && line[i] == line[i - 1])
            double_letter = true;

        if (i > 0 && ((line[i] == 'b' && line[i - 1] == 'a') ||
                      (line[i] == 'd' && line[i - 1] == 'c') ||
                      (line[i] == 'q' && line[i - 1] == 'p') ||
                      (line[i] == 'y' && line[i - 1] == 'x')))
        {
            bad_string = true;
            break; // Early exit if a bad string is found
        }
    }

    return vowels >= 3 && double_letter && !bad_string;
}

bool check_good_string_part2(const char *line)
{
    bool pair = false;
    bool repeat = false;
    size_t len = strlen(line);

    for (size_t i = 0; i < len - 1; i++)
    {
        // Check for repeating pairs
        for (size_t j = i + 2; j < len - 1; j++)
        {
            if (line[i] == line[j] && line[i + 1] == line[j + 1])
            {
                pair = true;
                break; // Early exit if a valid pair is found
            }
        }

        // Check for repeating letters
        if (i < len - 2 && line[i] == line[i + 2])
            repeat = true;
    }

    return pair && repeat;
}
