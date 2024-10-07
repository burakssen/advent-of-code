cmake_minimum_required(VERSION 3.30)
project(main)

set(CMAKE_CXX_STANDARD 23)

add_executable(main main.cpp)

target_include_directories(main PRIVATE /opt/homebrew/Cellar/openssl@3/3.3.2/include)
target_link_directories(main PRIVATE /opt/homebrew/Cellar/openssl@3/3.3.2/lib)
target_link_libraries(main PRIVATE ssl crypto)

add_custom_target(run
    COMMAND main ${CMAKE_SOURCE_DIR}/../../../inputs/2016/17.txt
    DEPENDS main
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)
