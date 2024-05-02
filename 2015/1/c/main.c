#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        printf("Usage: %s <input_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    char filename[20];
    sprintf(filename, "%s", argv[1]);

    FILE *file = fopen(filename, "r");

    if (file == NULL)
    {
        printf("Error: Could not open file %s\n", filename);
        return EXIT_FAILURE;
    }

    char *buffer = NULL;
    buffer = (char *)malloc(1000 * sizeof(char));

    if (buffer == NULL)
    {
        printf("Error: Could not allocate memory\n");
        return EXIT_FAILURE;
    }

    int floor = 0;
    int basement = 0;
    int count = 0;
    // Read the file
    while (fgets(buffer, 1000, file) != NULL)
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

    printf("Floor: %d\n", floor);
    printf("Basement: %d\n", basement);

    free(buffer);

    fclose(file);

    return EXIT_SUCCESS;
}
