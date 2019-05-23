cmake_minimum_required(VERSION 3.0.2)
project(GOTO_TEST)

set(CMAKE_C_FLAGS "$ENV{CFLAGS} -std=c99 -fPIC -Wall -Wno-deprecated -Winline")
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS} -g -ggdb")
set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS} -O3")

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

add_executable(exer5.23.v1
  exer5.23.v1.c)

add_executable(exer5.23.v2
  exer5.23.v2.c)
