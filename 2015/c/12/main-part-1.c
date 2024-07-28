#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

char *read_file(const char *filename)
{
    FILE *file = fopen(filename, "r");
    if (!file)
    {
        fprintf(stderr, "Error: cannot open file\n");
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *content = malloc(length + 1);
    if (!content)
    {
        fprintf(stderr, "Error: not enough memory to read file\n");
        fclose(file);
        return NULL;
    }

    fread(content, 1, length, file);
    fclose(file);
    content[length] = '\0';

    return content;
}

int sum_numbers_from_string(const char *str)
{
    int sum = 0;
    int i = 0;
    while (str[i])
    {
        // Skip non-digit characters except for '-' which might indicate a negative number
        while (str[i] && !isdigit(str[i]) && str[i] != '-')
        {
            i++;
        }

        // If we have found a digit or a '-', read the number
        if (isdigit(str[i]) || str[i] == '-')
        {
            char *end;
            int num = strtod(&str[i], &end);
            sum += num;
            i = end - str; // Move index to the end of the number
        }
    }

    return sum;
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        fprintf(stderr, "Error: missing input file\n");
        return EXIT_FAILURE;
    }

    char *filename = argv[1];
    char *json_content = read_file(filename);
    if (!json_content)
    {
        return EXIT_FAILURE;
    }

    int total_sum = sum_numbers_from_string(json_content);
    printf("Part 1: %d\n", total_sum);

    free(json_content);
    return EXIT_SUCCESS;
}
