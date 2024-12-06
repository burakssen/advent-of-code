#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "r");
    if (!file)
    {
        fprintf(stderr, "Error: Could not open file %s\n", argv[1]);
        return EXIT_FAILURE;
    }

    int floor = 0, basement = 0, count = 0;
    char ch;

    while ((ch = fgetc(file)) != EOF)
    {
        count++;
        if (ch == '(')
            floor++;
        else if (ch == ')')
            floor--;

        if (floor == -1 && basement == 0)
            basement = count;
    }

    printf("Part 1: %d\n", floor);
    printf("Part 2: %d\n", basement);

    fclose(file);
    return EXIT_SUCCESS;
}
