#include <iostream>
#include <fstream>
#include <string>
#include <openssl/md5.h>
#include <array>

// Function to compute the MD5 hash of a given input string
std::string md5(const std::string &input)
{
    unsigned char result[MD5_DIGEST_LENGTH];
    MD5(reinterpret_cast<const unsigned char *>(input.c_str()), input.length(), result);

    std::array<char, MD5_DIGEST_LENGTH * 2 + 1> hashStr;
    for (size_t i = 0; i < MD5_DIGEST_LENGTH; ++i)
    {
        snprintf(&hashStr[i * 2], 3, "%02x", result[i]);
    }
    return std::string(hashStr.data());
}

// Function to check if a hash contains three of the same character in a row
bool hasTriple(const std::string &hash, char &tripleChar)
{
    for (size_t i = 0; i < hash.length() - 2; ++i)
    {
        if (hash[i] == hash[i + 1] && hash[i] == hash[i + 2])
        {
            tripleChar = hash[i];
            return true;
        }
    }
    return false;
}

// Function to check if a hash contains five of the given character in a row
bool hasFiveInARow(const std::string &hash, char c)
{
    std::string five(5, c);
    return hash.find(five) != std::string::npos;
}

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

    std::string salt;
    std::getline(file, salt);
    file.close();

    int keyCount = 0;
    int index = 0;
    const int cacheSize = 1001;
    std::array<std::string, cacheSize> hashCache;

    while (keyCount < 64)
    {
        // Compute hash for the current index if not cached
        if (hashCache[index % cacheSize].empty())
        {
            hashCache[index % cacheSize] = md5(salt + std::to_string(index));
        }
        std::string &hash = hashCache[index % cacheSize];

        char tripleChar;
        if (hasTriple(hash, tripleChar))
        {
            // Precompute the next 1000 hashes if needed
            bool foundFive = false;
            for (int i = 1; i <= 1000 && !foundFive; ++i)
            {
                int checkIndex = (index + i) % cacheSize;
                if (hashCache[checkIndex].empty())
                {
                    hashCache[checkIndex] = md5(salt + std::to_string(index + i));
                }
                if (hasFiveInARow(hashCache[checkIndex], tripleChar))
                {
                    ++keyCount;
                    foundFive = true;
                }
            }
        }

        // Move to the next index
        hashCache[index % cacheSize].clear(); // Clear the current hash slot
        ++index;
    }

    std::cout << "Part 1: " << index - 1 << std::endl;
    return EXIT_SUCCESS;
}
