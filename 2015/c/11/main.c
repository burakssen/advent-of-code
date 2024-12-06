#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void increment_password(char *password)
{
    int i = strlen(password) - 1;
    while (i >= 0)
    {
        if (password[i] == 'z')
        {
            password[i] = 'a';
            i--;
        }
        else
        {
            password[i]++;
            break;
        }
    }
}

int has_increasing_straight(const char *password)
{
    for (int i = 0; i < (int)strlen(password) - 2; i++)
    {
        if (password[i + 1] == password[i] + 1 && password[i + 2] == password[i] + 2)
        {
            return 1;
        }
    }
    return 0;
}

int no_disallowed_chars(const char *password)
{
    return strchr(password, 'i') == NULL && strchr(password, 'o') == NULL && strchr(password, 'l') == NULL;
}

int has_two_pairs(const char *password)
{
    int pair_count = 0;
    for (int i = 0; i < (int)strlen(password) - 1; i++)
    {
        if (password[i] == password[i + 1])
        {
            pair_count++;
            i++; // Skip the next character to ensure pairs do not overlap
        }
    }
    return pair_count >= 2;
}

int is_valid(const char *password)
{
    return has_increasing_straight(password) && no_disallowed_chars(password) && has_two_pairs(password);
}

void find_next_password(char *password)
{
    do
    {
        increment_password(password);
    } while (!is_valid(password));
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

    char password[9];
    if (fscanf(file, "%8s", password) != 1)
    {
        fprintf(stderr, "Error: failed to read password from file\n");
        fclose(file);
        return EXIT_FAILURE;
    }
    fclose(file);

    find_next_password(password);
    printf("Part 1: %s\n", password);
    find_next_password(password);
    printf("Part 2: %s\n", password);

    return EXIT_SUCCESS;
}
