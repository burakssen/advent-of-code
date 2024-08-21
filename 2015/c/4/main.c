#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <openssl/evp.h>
#include <stdbool.h>

#define PREFIX_SIZE 9
#define INPUT_SIZE 50 // Buffer size for input string

int main(int argc, char **argv)
{
    if (argc != 2)
    {
        fprintf(stderr, "Usage: %s <input.txt>\n", argv[0]);
        return EXIT_FAILURE;
    }

    char prefix[PREFIX_SIZE];
    FILE *file = fopen(argv[1], "r");
    if (!file)
    {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    if (!fgets(prefix, PREFIX_SIZE, file))
    {
        perror("Error reading file");
        fclose(file);
        return EXIT_FAILURE;
    }
    fclose(file);

    prefix[strcspn(prefix, "\n")] = '\0'; // Remove newline character if present

    unsigned char md[EVP_MAX_MD_SIZE];
    unsigned int md_len;
    EVP_MD_CTX *mdctx = EVP_MD_CTX_new();
    const EVP_MD *mdtype = EVP_md5();
    bool found_5_leading = false;
    int i = 0;

    while (true)
    {
        char input[INPUT_SIZE];
        snprintf(input, sizeof(input), "%s%d", prefix, i);

        EVP_DigestInit_ex(mdctx, mdtype, NULL);
        EVP_DigestUpdate(mdctx, input, strlen(input));
        EVP_DigestFinal_ex(mdctx, md, &md_len);

        // Check for 6 leading zeros
        if (md[0] == 0 && md[1] == 0 && md[2] == 0)
        {
            printf("Part 2: %d\n", i);
            break;
        }

        // Check for 5 leading zeros
        if (md[0] == 0 && md[1] == 0 && (md[2] & 0xF0) == 0 && !found_5_leading)
        {
            found_5_leading = true;
            printf("Part 1: %d\n", i);
        }

        i++;
    }

    EVP_MD_CTX_free(mdctx);
    return EXIT_SUCCESS;
}
