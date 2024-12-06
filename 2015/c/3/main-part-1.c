#include <stdio.h>
#include <stdlib.h>

#define OFFSET 500
#define GRID_SIZE 1000

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
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    // Initialize the grid and starting position
    int x = 0, y = 0;
    int grid[GRID_SIZE][GRID_SIZE] = {0};
    grid[x + OFFSET][y + OFFSET] = 1;

    // Process the file
    int c;
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
        }

        if (x + OFFSET >= 0 && x + OFFSET < GRID_SIZE && y + OFFSET >= 0 && y + OFFSET < GRID_SIZE)
        {
            grid[x + OFFSET][y + OFFSET]++;
        }
    }

    // Count unique positions visited
    int count = 0;
    for (int i = 0; i < GRID_SIZE; i++)
    {
        for (int j = 0; j < GRID_SIZE; j++)
        {
            if (grid[i][j] > 0)
            {
                count++;
            }
        }
    }

    printf("Part 1: %d\n", count);

    fclose(file);
    return EXIT_SUCCESS;
}
