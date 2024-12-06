#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <openssl/evp.h>

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cerr << "Usage: " << argv[0] << " <input_file>\n";
        return EXIT_FAILURE;
    }

    std::ifstream file(argv[1]);
    if (!file)
    {
        std::cerr << "Error: Could not open file " << argv[1] << "\n";
        return EXIT_FAILURE;
    }

    std::string door_id;
    std::getline(file, door_id);
    file.close();

    std::string password1;
    std::vector<char> password2(8, ' ');

    int index = 0;
    int filled_positions = 0;

    // Initialize OpenSSL once
    OpenSSL_add_all_digests();
    const EVP_MD *md = EVP_get_digestbyname("md5");
    if (!md)
    {
        std::cerr << "Error: Unknown message digest\n";
        return EXIT_FAILURE;
    }

    EVP_MD_CTX *mdctx = EVP_MD_CTX_create();

    while (password1.size() < 8 || filled_positions < 8)
    {
        std::string hash_input = door_id + std::to_string(index);

        // Reuse the context and reset it for each iteration
        EVP_DigestInit_ex(mdctx, md, NULL);
        EVP_DigestUpdate(mdctx, hash_input.c_str(), hash_input.size());

        unsigned char hash[EVP_MAX_MD_SIZE];
        unsigned int hash_len;
        EVP_DigestFinal_ex(mdctx, hash, &hash_len);

        // Directly check the first few bytes of the hash for "00000"
        if (hash[0] == 0 && hash[1] == 0 && (hash[2] & 0xF0) == 0)
        {
            if (password1.size() < 8)
            {
                password1 += (hash[2] & 0x0F) < 10 ? (hash[2] & 0x0F) + '0' : (hash[2] & 0x0F) - 10 + 'a';
            }

            int position = hash[2] & 0x0F;
            if (position < 8 && password2[position] == ' ')
            {
                char character = (hash[3] & 0xF0) >> 4;
                password2[position] = character < 10 ? character + '0' : character - 10 + 'a';
                filled_positions++;
            }
        }

        index++;
    }

    EVP_MD_CTX_destroy(mdctx);

    std::cout << "Part 1: " << password1 << "\n";

    std::cout << "Part 2: ";
    for (char c : password2)
    {
        std::cout << c;
    }
    std::cout << "\n";

    return EXIT_SUCCESS;
}
