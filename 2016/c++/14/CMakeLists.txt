cmake_minimum_required(VERSION 3.30)
project(main)

set(CMAKE_CXX_STANDARD 23)

add_executable(main-part-1 main-part-1.cpp)
add_executable(main-part-2 main-part-2.cpp)

target_include_directories(main-part-1 PRIVATE /opt/homebrew/Cellar/openssl@3/3.3.2/include)
target_link_directories(main-part-1 PRIVATE /opt/homebrew/Cellar/openssl@3/3.3.2/lib)
target_link_libraries(main-part-1 PRIVATE ssl crypto pthread)

target_include_directories(main-part-2 PRIVATE /opt/homebrew/Cellar/openssl@3/3.3.2/include)
target_link_directories(main-part-2 PRIVATE /opt/homebrew/Cellar/openssl@3/3.3.2/lib)
target_link_libraries(main-part-2 PRIVATE ssl crypto pthread)

add_custom_target(run
    COMMAND main-part-1 ${CMAKE_SOURCE_DIR}/../../../inputs/2016/14.txt
    COMMAND main-part-2 ${CMAKE_SOURCE_DIR}/../../../inputs/2016/14.txt
    DEPENDS main-part-1 main-part-2
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)

