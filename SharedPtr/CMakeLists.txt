set(CMAKE_CXX_STANDARD 23)

foreach(target "simple-test")
  add_executable("SharedPtr-${target}" "${target}.cpp")
  target_link_libraries("SharedPtr-${target}" PRIVATE Catch2::Catch2WithMain)
endforeach()