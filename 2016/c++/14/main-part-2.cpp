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

    std::string hashStr;
    hashStr.reserve(MD5_DIGEST_LENGTH * 2);
    for (size_t i = 0; i < MD5_DIGEST_LENGTH; ++i)
    {
        char buf[3];
        snprintf(buf, sizeof(buf), "%02x", result[i]);
        hashStr.append(buf);
    }
    return hashStr;
}

// Function to perform key stretching: hashes the result 2016 additional times
std::string stretchedHash(const std::string &input)
{
    std::string hash = md5(input);
    for (int i = 0; i < 2016; ++i)
    {
        hash = md5(hash);
    }
    return hash;
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
    int count = 0;
    for (char ch : hash)
    {
        count = (ch == c) ? count + 1 : 0;
        if (count == 5)
        {
            return true;
        }
    }
    return false;
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
        int currentCacheIndex = index % cacheSize;

        // Compute stretched hash for the current index if not cached
        if (hashCache[currentCacheIndex].empty())
        {
            hashCache[currentCacheIndex] = stretchedHash(salt + std::to_string(index));
        }
        std::string &hash = hashCache[currentCacheIndex];

        char tripleChar;
        if (hasTriple(hash, tripleChar))
        {
            // Precompute the next 1000 hashes if needed
            for (int i = 1; i <= 1000; ++i)
            {
                int checkIndex = (index + i) % cacheSize;
                if (hashCache[checkIndex].empty())
                {
                    hashCache[checkIndex] = stretchedHash(salt + std::to_string(index + i));
                }
                if (hasFiveInARow(hashCache[checkIndex], tripleChar))
                {
                    ++keyCount;
                    break; // Found a valid key, break out of the loop
                }
            }
        }

        // Clear the hash slot for the next round
        hashCache[currentCacheIndex].clear();
        ++index;
    }

    std::cout << "Part 2: " << index - 1 << std::endl;
    return EXIT_SUCCESS;
}
