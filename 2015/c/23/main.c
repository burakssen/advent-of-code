#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_INSTRUCTIONS 1000
#define MAX_LINE_LENGTH 20

typedef struct
{
    char instruction[4];
    char reg;
    int offset;
} Instruction;

Instruction program[MAX_INSTRUCTIONS];
int program_size = 0;
int reg_a = 0, reg_b = 0;

void execute_program(void)
{
    int pc = 0;
    while (pc >= 0 && pc < program_size)
    {
        Instruction *instr = &program[pc];
        int *reg = (instr->reg == 'a') ? &reg_a : &reg_b;

        if (strcmp(instr->instruction, "hlf") == 0)
        {
            *reg /= 2;
            pc++;
        }
        else if (strcmp(instr->instruction, "tpl") == 0)
        {
            *reg *= 3;
            pc++;
        }
        else if (strcmp(instr->instruction, "inc") == 0)
        {
            (*reg)++;
            pc++;
        }
        else if (strcmp(instr->instruction, "jmp") == 0)
        {
            pc += instr->offset;
        }
        else if (strcmp(instr->instruction, "jie") == 0)
        {
            if (*reg % 2 == 0)
            {
                pc += instr->offset;
            }
            else
            {
                pc++;
            }
        }
        else if (strcmp(instr->instruction, "jio") == 0)
        {
            if (*reg == 1)
            {
                pc += instr->offset;
            }
            else
            {
                pc++;
            }
        }
    }
}

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    char *filename = argv[1];

    FILE *file = fopen(filename, "r");
    if (file == NULL)
    {
        perror("Error opening file");
        exit(1);
    }

    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), file) && program_size < MAX_INSTRUCTIONS)
    {
        Instruction *instr = &program[program_size++];
        sscanf(line, "%s %c, %d", instr->instruction, &instr->reg, &instr->offset);
        if (strcmp(instr->instruction, "jmp") == 0)
        {
            sscanf(line, "%s %d", instr->instruction, &instr->offset);
        }
    }

    fclose(file);

    reg_a = 0;
    reg_b = 0;
    execute_program();
    printf("Part 1: %d\n", reg_b);

    reg_a = 1;
    reg_b = 0;
    execute_program();
    printf("Part 2: %d\n", reg_b);

    return 0;
}
