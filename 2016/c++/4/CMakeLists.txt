cmake_minimum_required(VERSION 3.30)
project(main)
set(CMAKE_CXX_STANDARD 23)
add_executable(main main.cpp)

add_custom_target(run
    COMMAND main ${CMAKE_SOURCE_DIR}/../../../inputs/2016/4.txt
    DEPENDS main
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)