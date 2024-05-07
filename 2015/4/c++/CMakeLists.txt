project(day4)

set(CMAKE_CXX_STANDARD 23)

add_executable(day4 main.cpp)

target_include_directories(day4 PUBLIC "/opt/homebrew/Cellar/openssl@3/3.3.0/include")
target_link_directories(day4 PUBLIC "/opt/homebrew/Cellar/openssl@3/3.3.0/lib")
target_link_libraries(day4 ssl crypto)