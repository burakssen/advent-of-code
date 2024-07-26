cmake_minimum_required(VERSION 3.30)
project(main)

set(CMAKE_CXX_STANDARD 23)

find_package(PkgConfig REQUIRED)
pkg_check_modules(OPENSSL REQUIRED openssl)

add_executable(main main.cpp)

target_include_directories(main PUBLIC ${OPENSSL_INCLUDE_DIRS})
target_link_directories(main PUBLIC ${OPENSSL_LIBRARY_DIRS})
target_link_libraries(main ${OPENSSL_LIBRARIES})

add_custom_target(run
    COMMAND main ${CMAKE_SOURCE_DIR}/../../../inputs/2015/4.txt
    DEPENDS main
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)
