#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SIZE 100

void update_table(char table[SIZE][SIZE]);
void copy_table(char dest[SIZE][SIZE], char src[SIZE][SIZE]);
void load_table_from_file(char table[SIZE][SIZE], const char *filename);
int count_lights_on(const char table[SIZE][SIZE]);

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        printf("Usage: %s <filename>\n", argv[0]);
        return 1;
    }

    char table[SIZE][SIZE];
    load_table_from_file(table, argv[1]);

    for (int k = 0; k < SIZE; k++)
    {
        update_table(table);
    }

    int count = count_lights_on(table);
    printf("Part 1: %d\n", count);

    load_table_from_file(table, argv[1]);
    table[0][0] = '#';
    table[0][SIZE - 1] = '#';
    table[SIZE - 1][0] = '#';
    table[SIZE - 1][SIZE - 1] = '#';

    for (int k = 0; k < SIZE; k++)
    {
        update_table(table);
        table[0][0] = '#';
        table[0][SIZE - 1] = '#';
        table[SIZE - 1][0] = '#';
        table[SIZE - 1][SIZE - 1] = '#';
    }

    count = count_lights_on(table);
    printf("Part 2: %d\n", count);

    return 0;
}

void update_table(char table[SIZE][SIZE])
{
    char new_table[SIZE][SIZE];
    int dx[] = {-1, -1, -1, 0, 0, 1, 1, 1};
    int dy[] = {-1, 0, 1, -1, 1, -1, 0, 1};

    for (int i = 0; i < SIZE; i++)
    {
        for (int j = 0; j < SIZE; j++)
        {
            int neighbors_on = 0;
            for (int k = 0; k < 8; k++)
            {
                int ni = i + dx[k], nj = j + dy[k];
                if (ni >= 0 && ni < SIZE && nj >= 0 && nj < SIZE && table[ni][nj] == '#')
                {
                    neighbors_on++;
                }
            }

            if (table[i][j] == '#')
            {
                new_table[i][j] = (neighbors_on == 2 || neighbors_on == 3) ? '#' : '.';
            }
            else
            {
                new_table[i][j] = (neighbors_on == 3) ? '#' : '.';
            }
        }
    }

    copy_table(table, new_table);
}

void copy_table(char dest[SIZE][SIZE], char src[SIZE][SIZE])
{
    for (int i = 0; i < SIZE; i++)
    {
        for (int j = 0; j < SIZE; j++)
        {
            dest[i][j] = src[i][j];
        }
    }
}

void load_table_from_file(char table[SIZE][SIZE], const char *filename)
{
    FILE *file = fopen(filename, "r");
    if (file == NULL)
    {
        perror("Error opening file");
        exit(1);
    }

    char line[SIZE + 2]; // +2 to account for the newline and null terminator
    int i = 0;
    while (fgets(line, sizeof(line), file) && i < SIZE)
    {
        line[strcspn(line, "\n")] = 0; // Remove newline character
        for (int j = 0; j < SIZE; j++)
        {
            table[i][j] = line[j];
        }
        i++;
    }

    fclose(file);
}

int count_lights_on(const char table[SIZE][SIZE])
{
    int count = 0;
    for (int i = 0; i < SIZE; i++)
    {
        for (int j = 0; j < SIZE; j++)
        {
            if (table[i][j] == '#')
            {
                count++;
            }
        }
    }
    return count;
}
