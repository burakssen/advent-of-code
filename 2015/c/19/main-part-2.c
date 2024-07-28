#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MAX_RULES 100
#define MAX_MOLECULE_LENGTH 1000

typedef struct
{
    char from[10];
    char to[10];
} Replacement;

Replacement rules[MAX_RULES];
int rule_count = 0;
char medicine[MAX_MOLECULE_LENGTH];

// Function to shuffle the rules array
void shuffle_rules(void)
{
    for (int i = rule_count - 1; i > 0; i--)
    {
        int j = rand() % (i + 1);
        Replacement temp = rules[i];
        rules[i] = rules[j];
        rules[j] = temp;
    }
}

// Function to perform backtracking
int backtrack(char *s)
{
    int count = 0;
    char old_s[MAX_MOLECULE_LENGTH];
    old_s[0] = '\0';

    shuffle_rules();

    while (strcmp(old_s, s) != 0)
    {
        strcpy(old_s, s);
        for (int i = 0; i < rule_count; i++)
        {
            char *pos = s;
            while ((pos = strstr(pos, rules[i].to)) != NULL)
            {
                count += 1;
                int to_len = strlen(rules[i].to);
                int from_len = strlen(rules[i].from);
                memmove(pos + from_len, pos + to_len, strlen(pos + to_len) + 1);
                memcpy(pos, rules[i].from, from_len);
                if (strcmp(s, "e") == 0)
                {
                    return count;
                }
                pos += from_len; // Move past the replacement
            }
        }
    }
    return 0;
}

int main(int argc, char *argv[])
{
    srand(time(NULL));

    if (argc != 3)
    {
        printf("Usage: %s <rule_filename> <medicine_filename>\n", argv[0]);
        return 1;
    }

    char *rule_filename = argv[1];
    char *medicine_filename = argv[2];

    FILE *rule_file = fopen(rule_filename, "r");
    if (rule_file == NULL)
    {
        perror(rule_filename);
        return 1;
    }

    FILE *medicine_file = fopen(medicine_filename, "r");
    if (medicine_file == NULL)
    {
        perror(medicine_filename);
        return 1;
    }

    // Read rules
    while (fscanf(rule_file, "%s => %s", rules[rule_count].from, rules[rule_count].to) == 2)
    {
        rule_count++;
    }
    fclose(rule_file);

    // Read medicine molecule
    if (fgets(medicine, MAX_MOLECULE_LENGTH, medicine_file) == NULL)
    {
        perror("Failed to read medicine molecule");
        return 1;
    }
    // Remove trailing newline
    medicine[strcspn(medicine, "\n")] = '\0';
    fclose(medicine_file);

    // Part Two: Backtracking to find the fewest steps
    int steps = 0;
    int attempts = 0;
    while (steps == 0 && attempts < 1000)
    { // Limit the number of attempts
        steps = backtrack(medicine);
        attempts++;
    }

    if (steps > 0)
    {
        printf("Part 2: %d\n", steps);
    }
    else
    {
        printf("Failed to find a solution in 1000 attempts.\n");
        printf("Try running the program again a couple of times.\n");
    }

    return 0;
}
