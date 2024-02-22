set(CMAKE_CXX_STANDARD 20)

add_executable(BigInteger-test test.cpp)
target_link_libraries(BigInteger-test PRIVATE Catch2::Catch2WithMain)