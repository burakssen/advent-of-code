cmake_minimum_required(VERSION 3.30)

project(main)

set(CMAKE_CXX_STANDARD 23)

add_executable(main-part-1 main-part-1.cpp)
add_executable(main-part-2 main-part-2.cpp)

add_custom_target(run
    COMMAND main-part-1 ${CMAKE_SOURCE_DIR}/../../../inputs/2015/6.txt
    COMMAND main-part-2 ${CMAKE_SOURCE_DIR}/../../../inputs/2015/6.txt
    DEPENDS main-part-1 main-part-2
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)
