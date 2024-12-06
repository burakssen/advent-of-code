#include <openssl/evp.h>
#include <iostream>
#include <fstream>
#include <string>

int main(int argc, char **argv)
{
    if (argc != 2)
    {
        std::cout << "Usage: " << argv[0] << " <input.txt>\n";
        return 1;
    }

    std::string file_name = argv[1];

    std::ifstream file(file_name);

    std::string prefix;
    std::getline(file, prefix);

    file.close();

    int i = 0;
    bool found_5_leading = false;

    while (true)
    {
        std::string input = prefix + std::to_string(i);

        unsigned char md[16];
        EVP_MD_CTX *mdctx;
        const EVP_MD *mdtype = EVP_md5();

        mdctx = EVP_MD_CTX_new();
        EVP_DigestInit_ex(mdctx, mdtype, NULL);
        EVP_DigestUpdate(mdctx, input.c_str(), input.size());
        EVP_DigestFinal_ex(mdctx, md, NULL);
        EVP_MD_CTX_free(mdctx);

        // the lowest positive number that produces a hash with 6 leading zeros
        if (md[0] == 0 && md[1] == 0 && md[2] == 0)
        {
            printf("Part 2: %d\n", i);
            break;
        }

        if (md[0] == 0 && md[1] == 0 && (md[2] & 0xF0) == 0 && !found_5_leading)
        {
            found_5_leading = true;
            printf("Part 1: %d\n", i);
        }

        i++;
    }

    return 0;
}
