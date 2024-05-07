#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <openssl/evp.h>
#include <stdbool.h>

int main(int argc, char **argv)
{

    if (argc != 2)
    {
        printf("Usage: %s <input.txt>\n", argv[0]);
        return 1;
    }

    char *filename = argv[1];

    FILE *file = fopen(filename, "r");

    char *prefix = (char *)malloc(sizeof(char) * 9);

    fgets(prefix, 9, file);

    fclose(file);

    int i = 0;

    bool found_5_leading = false;

    while (true)
    {
        char *input = malloc(100);
        sprintf(input, "%s%d", prefix, i);

        unsigned char md[16];
        EVP_MD_CTX *mdctx;
        const EVP_MD *mdtype = EVP_md5();

        mdctx = EVP_MD_CTX_new();
        EVP_DigestInit_ex(mdctx, mdtype, NULL);
        EVP_DigestUpdate(mdctx, input, strlen(input));
        EVP_DigestFinal_ex(mdctx, md, NULL);
        EVP_MD_CTX_free(mdctx);

        // the lowest positive number that produces a hash with 6 leading zeros
        if (md[0] == 0 && md[1] == 0 && md[2] == 0)
        {
            printf("6 Zeros: %d\n", i);
            break;
        }

        if (md[0] == 0 && md[1] == 0 && (md[2] & 0xF0) == 0 && !found_5_leading)
        {
            found_5_leading = true;
            printf("5 Zeros: %d\n", i);
        }

        i++;
    }

    return 0;
}
