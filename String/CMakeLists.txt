set(CMAKE_CXX_STANDARD 17)

add_executable(String-test test.cpp)
target_link_libraries(String-test PRIVATE Catch2::Catch2WithMain)