#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE_LENGTH 10000

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "r");
    if (file == NULL)
    {
        fprintf(stderr, "Error: Could not open file %s\n", argv[1]);
        return EXIT_FAILURE;
    }

    char *buffer = malloc(MAX_LINE_LENGTH * sizeof(char));
    if (buffer == NULL)
    {
        fprintf(stderr, "Error: Could not allocate memory\n");
        fclose(file);
        return EXIT_FAILURE;
    }

    int floor = 0;
    int basement = 0;
    int count = 0;

    // Read the file
    while (fgets(buffer, MAX_LINE_LENGTH, file) != NULL)
    {
        for (int i = 0; buffer[i] != '\0'; i++)
        {
            if (buffer[i] == '(')
            {
                floor++;
            }
            else if (buffer[i] == ')')
            {
                floor--;
            }
            count++;
            if (floor == -1 && basement == 0)
            {
                basement = count;
            }
        }
    }

    printf("Part 1: %d\n", floor);
    printf("Part 2: %d\n", basement);

    free(buffer);
    fclose(file);
    return EXIT_SUCCESS;
}
