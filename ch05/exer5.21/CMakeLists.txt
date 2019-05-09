cmake_minimum_required(VERSION 2.6)
project(PROC_LANG)

set(CMAKE_C_FLAGS "$ENV{CFLAGS} -std=c99 -fPIC -Wall -Wno-deprecated -Winline")
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS} -g -ggdb")
set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS} -O3")

find_package(BISON)
find_package(FLEX)

bison_target(PROC_PARSER proc.y
  ${CMAKE_CURRENT_BINARY_DIR}/proc_parser.c
  DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/proc_parser.h)

flex_target(PROC_SCANNER proc.l
  ${CMAKE_CURRENT_BINARY_DIR}/proc_scanner.c
  DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/proc_scanner.h)

add_flex_bison_dependency(PROC_SCANNER PROC_PARSER)

include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

add_executable(proc
  proc.c
  ${BISON_PROC_PARSER_OUTPUTS}
  ${FLEX_PROC_SCANNER_OUTPUTS})
