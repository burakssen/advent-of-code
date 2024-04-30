#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        printf("Usage: %s <input_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    char *filename = argv[1];

    FILE *file = fopen(filename, "r");
    if (file == NULL)
    {
        printf("Error: Could not open file %s\n", filename);
        return EXIT_FAILURE;
    }

    int x = 0, y = 0;

    char c;

    int grid[1000][1000] = {0};
    grid[x + 499][y + 499] = 1;

    while ((c = fgetc(file)) != EOF)
    {
        switch (c)
        {
        case '^':
            y++;
            break;
        case 'v':
            y--;
            break;
        case '>':
            x++;
            break;
        case '<':
            x--;
            break;
        default:
            break;
        }

        grid[x + 499][y + 499]++;
    }

    int count = 0;
    for (int i = 0; i < 1000; i++)
    {
        for (int j = 0; j < 1000; j++)
        {
            if (grid[i][j] > 0)
            {
                count++;
            }
        }
    }

    printf("%d\n", count);

    fclose(file);
    return EXIT_SUCCESS;
}
