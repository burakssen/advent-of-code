#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>

#define HASH_TABLE_SIZE 1000
#define MAX_LINE_LENGTH 256

typedef enum
{
    AND,
    OR,
    NOT,
    LSHIFT,
    RSHIFT,
    ASSIGN
} InstructionType;

typedef struct
{
    InstructionType type;
    char op1[10];
    char op2[10];
    bool is_op2;
    char output[10];
} instruction;

instruction instructions[HASH_TABLE_SIZE];

typedef struct Entry
{
    char *key;
    int value;
    struct Entry *next;
} Entry;

Entry *hashTable[HASH_TABLE_SIZE];

// hashtable functions
void create_hash_table(void);
int hash(const char *key);
Entry *create_entry(const char *key, int value);
int insert_entry(const char *key, int value);
Entry *get_entry(const char *key);
void free_hash_table(void);

// instruction functions
void insert_instruction(char *line, int i);
instruction *get_instruction(const char *str);
void set_instruction_val(const char *str, char *val);
unsigned short eval(instruction *instr);

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
        fprintf(stderr, "Error: could not open file %s\n", filename);
        return EXIT_FAILURE;
    }

    char line[MAX_LINE_LENGTH];
    int i = 0;
    while (fgets(line, sizeof(line), file))
    {
        insert_instruction(line, i);
        i++;
    }

    fclose(file);

    int signal_a = eval(get_instruction("a"));
    printf("Part 1: %d\n", signal_a);

    free_hash_table();

    get_instruction("b")->type = ASSIGN;

    char val[10];
    sprintf(val, "%d", signal_a);
    set_instruction_val("b", val);

    signal_a = eval(get_instruction("a"));
    printf("Part 2: %d\n", signal_a);

    free_hash_table();

    return EXIT_SUCCESS;
}

void create_hash_table(void)
{
    for (int i = 0; i < HASH_TABLE_SIZE; i++)
    {
        hashTable[i] = NULL;
    }
}

int hash(const char *key)
{
    unsigned int h = 0;
    for (size_t i = 0; i < strlen(key); i++)
    {
        h = (h * 0x5bd1e995) ^ key[i];
    }
    return h % HASH_TABLE_SIZE;
}

Entry *create_entry(const char *key, int value)
{
    Entry *entry = malloc(sizeof(Entry));
    if (entry == NULL)
    {
        fprintf(stderr, "Error: failed to allocate memory\n");
        exit(EXIT_FAILURE);
    }
    entry->key = strdup(key);
    if (entry->key == NULL)
    {
        fprintf(stderr, "Error: failed to allocate memory\n");
        free(entry);
        exit(EXIT_FAILURE);
    }
    entry->value = value;
    entry->next = NULL;
    return entry;
}

int insert_entry(const char *key, int value)
{
    int index = hash(key);
    Entry *entry = hashTable[index];

    while (entry)
    {
        if (strcmp(entry->key, key) == 0)
        {
            entry->value = value;
            return value;
        }
        entry = entry->next;
    }

    Entry *new_entry = create_entry(key, value);
    new_entry->next = hashTable[index];
    hashTable[index] = new_entry;

    return value;
}

Entry *get_entry(const char *key)
{
    int index = hash(key);
    Entry *entry = hashTable[index];

    while (entry)
    {
        if (entry->key && strcmp(entry->key, key) == 0)
        {
            return entry;
        }
        entry = entry->next;
    }

    return NULL;
}

void free_hash_table(void)
{
    for (int i = 0; i < HASH_TABLE_SIZE; i++)
    {
        Entry *entry = hashTable[i];
        while (entry)
        {
            Entry *temp = entry;
            entry = entry->next;
            free(temp->key);
            free(temp);
        }
        hashTable[i] = NULL;
    }
}

void insert_instruction(char *line, int i)
{
    instructions[i].is_op2 = false;
    if (strstr(line, "AND"))
    {
        sscanf(line, "%7s AND %7s -> %7s", instructions[i].op1, instructions[i].op2, instructions[i].output);
        instructions[i].type = AND;
        instructions[i].is_op2 = true;
    }
    else if (strstr(line, "OR"))
    {
        sscanf(line, "%7s OR %7s -> %7s", instructions[i].op1, instructions[i].op2, instructions[i].output);
        instructions[i].type = OR;
        instructions[i].is_op2 = true;
    }
    else if (strstr(line, "NOT"))
    {
        sscanf(line, "NOT %7s -> %7s", instructions[i].op1, instructions[i].output);
        instructions[i].type = NOT;
    }
    else if (strstr(line, "LSHIFT"))
    {
        sscanf(line, "%7s LSHIFT %7s -> %7s", instructions[i].op1, instructions[i].op2, instructions[i].output);
        instructions[i].type = LSHIFT;
        instructions[i].is_op2 = true;
    }
    else if (strstr(line, "RSHIFT"))
    {
        sscanf(line, "%7s RSHIFT %7s -> %7s", instructions[i].op1, instructions[i].op2, instructions[i].output);
        instructions[i].type = RSHIFT;
        instructions[i].is_op2 = true;
    }
    else
    {
        sscanf(line, "%7s -> %7s", instructions[i].op1, instructions[i].output);
        instructions[i].type = ASSIGN;
    }
}

instruction *get_instruction(const char *str)
{
    for (int i = 0; i < HASH_TABLE_SIZE; i++)
    {
        if (strcmp(instructions[i].output, str) == 0)
        {
            return &instructions[i];
        }
    }
    return NULL;
}

void set_instruction_val(const char *str, char *val)
{
    for (int i = 0; i < HASH_TABLE_SIZE; i++)
    {
        if (strcmp(instructions[i].output, str) == 0)
        {
            strcpy(instructions[i].op1, val);
            break;
        }
    }
}

unsigned short eval(instruction *instr)
{
    Entry *entry = get_entry(instr->op1);
    if (instr->type == 5 && entry != NULL)
    {
        return entry->value;
    }

    int op1 = isdigit(instr->op1[0]) ? atoi(instr->op1) : eval(get_instruction(instr->op1));
    int op2 = 0;

    if (instr->is_op2)
    {
        entry = get_entry(instr->op2);
        if (entry != NULL)
            op2 = entry->value;
        else
            op2 = isdigit(instr->op2[0]) ? atoi(instr->op2) : eval(get_instruction(instr->op2));
    }

    switch (instr->type)
    {
    case ASSIGN:
        return insert_entry(instr->output, op1);
    case RSHIFT:
        return insert_entry(instr->output, op1 >> op2);
    case LSHIFT:
        return insert_entry(instr->output, op1 << op2);
    case NOT:
        return insert_entry(instr->output, ~op1);
    case OR:
        return insert_entry(instr->output, op1 | op2);
    case AND:
        return insert_entry(instr->output, op1 & op2);
    default:
        fprintf(stderr, "Error: invalid instruction type\n");
        exit(EXIT_FAILURE);
    }
}
