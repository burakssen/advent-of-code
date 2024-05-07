#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

bool check_good_string_part1(char *line, int len);
bool check_good_string_part2(char *line, int len);

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        fprintf(stderr, "Usage: %s <input.txt>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "r");

    if (file == NULL)
    {
        fprintf(stderr, "Error: Could not open file %s\n", argv[1]);
        return EXIT_FAILURE;
    }

    char *line = NULL;
    size_t len = 0;
    int part1_count = 0;
    int part2_count = 0;

    while (getline(&line, &len, file) != -1)
    {
        // Remove newline character
        line[strcspn(line, "\n")] = 0;
        len = strlen(line);

        if (check_good_string_part1(line, len))
            part1_count++;

        if (check_good_string_part2(line, len))
            part2_count++;
    }

    printf("%d\n", part1_count);
    printf("%d\n", part2_count);
    free(line);
    fclose(file);
    return EXIT_SUCCESS;
}

bool check_good_string_part1(char *line, int len)
{
    int vowels = 0;
    bool double_letter = false;
    bool bad_string = false;

    for (int i = 0; i < len; i++)
    {
        if (line[i] == 'a' || line[i] == 'e' || line[i] == 'i' || line[i] == 'o' || line[i] == 'u')
            vowels++;

        if (i > 0 && line[i] == line[i - 1])
            double_letter = true;

        if (i > 0 && ((line[i] == 'b' && line[i - 1] == 'a') || (line[i] == 'd' && line[i - 1] == 'c') || (line[i] == 'q' && line[i - 1] == 'p') || (line[i] == 'y' && line[i - 1] == 'x')))
            bad_string = true;
    }

    return vowels >= 3 && double_letter && !bad_string;
}

bool check_good_string_part2(char *line, int len)
{
    bool pair = false;
    bool repeat = false;

    for (int i = 0; i < len - 1; i++)
    {
        for (int j = i + 2; j < len - 1; j++)
        {
            if (line[i] == line[j] && line[i + 1] == line[j + 1])
                pair = true;
        }

        if (i < len - 2 && line[i] == line[i + 2])
            repeat = true;
    }

    return pair && repeat;
}
