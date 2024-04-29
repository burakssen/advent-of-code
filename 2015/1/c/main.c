#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        printf("Usage: %s <number>\n", argv[0]);
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

    return EXIT_SUCCESS;
}