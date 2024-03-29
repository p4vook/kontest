set(CMAKE_CXX_STANDARD 23)

foreach(target "mesherin-test")
  add_executable("UnorderedMap-${target}" "${target}.cpp")
  target_link_libraries("UnorderedMap-${target}" PRIVATE Catch2::Catch2WithMain)
endforeach()