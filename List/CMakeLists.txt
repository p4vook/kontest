set(CMAKE_CXX_STANDARD 23)

foreach(target "mesherin-test")
  add_executable("List-${target}" "${target}.cpp")
  target_link_libraries("List-${target}" PRIVATE Catch2::Catch2WithMain)
endforeach()