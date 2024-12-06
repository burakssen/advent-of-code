#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cJSON/cJSON.h" // Ensure you have cJSON library installed and include its header

// Function to read file content into a string
char *read_file(const char *filename)
{
    FILE *file = fopen(filename, "r");
    if (!file)
    {
        fprintf(stderr, "Error: cannot open file\n");
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *content = malloc(length + 1);
    if (!content)
    {
        fprintf(stderr, "Error: not enough memory to read file\n");
        fclose(file);
        return NULL;
    }

    fread(content, 1, length, file);
    fclose(file);
    content[length] = '\0';

    return content;
}

// Function to check if any property of a cJSON object has the value "red"
int has_red_property(cJSON *object)
{
    cJSON *child = object->child;
    while (child)
    {
        if (cJSON_IsString(child) && strcmp(child->valuestring, "red") == 0)
        {
            return 1;
        }
        child = child->next;
    }
    return 0;
}

// Recursive function to sum all numbers in a cJSON object, ignoring objects with a "red" property
int sum_numbers(cJSON *item)
{
    int sum = 0;

    if (cJSON_IsNumber(item))
    {
        sum += item->valuedouble;
    }
    else if (cJSON_IsObject(item))
    {
        if (has_red_property(item))
        {
            return 0;
        }
        cJSON *child = item->child;
        while (child)
        {
            sum += sum_numbers(child);
            child = child->next;
        }
    }
    else if (cJSON_IsArray(item))
    {
        cJSON *child = item->child;
        while (child)
        {
            sum += sum_numbers(child);
            child = child->next;
        }
    }

    return sum;
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        fprintf(stderr, "Error: missing input file\n");
        return EXIT_FAILURE;
    }

    char *filename = argv[1];
    char *json_content = read_file(filename);
    if (!json_content)
    {
        return EXIT_FAILURE;
    }

    cJSON *json = cJSON_Parse(json_content);
    if (!json)
    {
        fprintf(stderr, "Error: failed to parse JSON\n");
        free(json_content);
        return EXIT_FAILURE;
    }

    int total_sum = sum_numbers(json);
    printf("Part 2: %d\n", total_sum);

    cJSON_Delete(json);
    free(json_content);

    return EXIT_SUCCESS;
}
