set(CMAKE_CXX_STANDARD 23)

foreach(target "helper-test" "matrix-test")
  add_executable("Matrix-${target}" "${target}.cpp")
  target_link_libraries("Matrix-${target}" PRIVATE Catch2::Catch2WithMain)
endforeach()