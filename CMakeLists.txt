cmake_minimum_required(VERSION 3.13.0)
set(CMAKE_COLOR_DIAGNOSTICS ON)
project("Kontest")

#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fuse-ld=mold")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall -Wextra -fsanitize=address,undefined,array-bounds -fno-inline -O0")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O2 -g")

find_package(Catch2 3 REQUIRED)

foreach(target "Stack" "String" "BigInteger" "Geometry" "Matrix" "Deque" "List") 
  add_subdirectory("${target}")
endforeach()