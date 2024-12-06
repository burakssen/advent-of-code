#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void count_characters(const char *input, int *code_chars, int *string_chars)
{
    int len = strlen(input);
    *code_chars += len;

    for (int i = 1; i < len - 1; i++)
    { // Skip first and last quote
        if (input[i] == '\\')
        {
            if (i + 1 < len - 1)
            {
                switch (input[i + 1])
                {
                case '\\':
                    (*string_chars)++;
                    i++;
                    break;
                case '"':
                    (*string_chars)++;
                    i++;
                    break;
                case 'x':
                    if (i + 3 < len - 1)
                    {
                        (*string_chars)++;
                        i += 3;
                    }
                    break;
                default:
                    (*string_chars)++;
                }
            }
        }
        else
        {
            (*string_chars)++;
        }
    }
}

int encode_string(const char *input, char *output)
{
    int j = 0;
    output[j++] = '"';

    for (int i = 0; input[i] != '\0'; i++)
    {
        if (input[i] == '"' || input[i] == '\\')
        {
            output[j++] = '\\';
        }
        output[j++] = input[i];
    }

    output[j++] = '"';
    output[j] = '\0';

    return j;
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

    int total_code_chars = 0;
    int total_string_chars = 0;

    int total_original_chars = 0;
    int total_encoded_chars = 0;

    char buffer[1024];
    char encoded[2048];
    while (fgets(buffer, sizeof(buffer), file) != NULL)
    {
        // Remove newline character if present
        buffer[strcspn(buffer, "\n")] = 0;

        // Part 1: Count characters
        count_characters(buffer, &total_code_chars, &total_string_chars);

        // Part 2: Encode string
        int original_len = strlen(buffer);
        total_original_chars += original_len;

        int encoded_len = encode_string(buffer, encoded);
        total_encoded_chars += encoded_len;
    }

    printf("Part 1: %d\n", total_code_chars - total_string_chars);
    printf("Part 2: %d\n", total_encoded_chars - total_original_chars);

    fclose(file);
    return EXIT_SUCCESS;
}
