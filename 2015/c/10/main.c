#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *count_and_say(const char *s)
{
    int len = strlen(s);
    char *result = malloc(2 * len + 1); // Allocate enough space for the result
    if (result == NULL)
    {
        perror("Unable to allocate memory");
        exit(EXIT_FAILURE);
    }

    int i = 0, j = 0;
    while (i < len)
    {
        char current_char = s[i];
        int count = 0;
        // Count occurrences of the current character
        while (i < len && s[i] == current_char)
        {
            count++;
            i++;
        }
        // Append the count and character to the result
        j += snprintf(result + j, 2 * len + 1 - j, "%d%c", count, current_char);
    }
    result[j] = '\0'; // Null-terminate the string
    return result;
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        fprintf(stderr, "Error: missing input file\n");
        return EXIT_FAILURE;
    }

    char *filename = argv[1];
    FILE *file = fopen(filename, "r");
    if (file == NULL)
    {
        fprintf(stderr, "Error: cannot open file\n");
        return EXIT_FAILURE;
    }

    // read the only line of the file
    char *line = malloc(11); // Allocate 100 bytes
    if (line == NULL)
    {
        perror("Unable to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    if (fgets(line, 11, file) == NULL)
    {
        fprintf(stderr, "Error: cannot read file or file is empty\n");
        free(line);
        fclose(file);
        return EXIT_FAILURE;
    }

    // Remove trailing newline character if present
    size_t len = strlen(line);
    if (len > 0 && line[len - 1] == '\n')
    {
        line[len - 1] = '\0';
    }

    for (int i = 0; i < 50; i++)
    {
        if (i == 40)
            printf("Part 1: %zu\n", strlen(line));
        char *next = count_and_say(line);
        free(line);
        line = next;
    }

    printf("Part 2: %zu\n", strlen(line));

    free(line);
    fclose(file);
    return EXIT_SUCCESS;
}
