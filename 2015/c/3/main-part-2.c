#include <stdio.h>
#include <stdlib.h>

struct Santa
{
    int x;
    int y;
};

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

    struct Santa santa = {0, 0};
    struct Santa robo_santa = {0, 0};

    char c;

    int grid[1000][1000] = {0};
    grid[santa.x + 499][santa.y + 499] = 1;
    grid[robo_santa.x + 499][robo_santa.y + 499] = 1;

    int santa_turn = 1;

    while ((c = fgetc(file)) != EOF)
    {
        struct Santa *current_santa = santa_turn ? &santa : &robo_santa;

        switch (c)
        {
        case '^':
            current_santa->y++;
            break;
        case 'v':
            current_santa->y--;
            break;
        case '>':
            current_santa->x++;
            break;
        case '<':
            current_santa->x--;
            break;
        default:
            break;
        }

        grid[current_santa->x + 499][current_santa->y + 499]++;
        santa_turn = !santa_turn;
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

    printf("Part 2: %d\n", count);

    return EXIT_SUCCESS;
}
