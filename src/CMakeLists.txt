cmake_minimum_required(VERSION 3.20)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED true)

project(basic LANGUAGES C)

add_executable(${PROJECT_NAME} basic-e.c)
