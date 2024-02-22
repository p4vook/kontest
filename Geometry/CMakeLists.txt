set(CMAKE_CXX_STANDARD 20)

foreach(target "primitive-test" "polygon-test" "ellipse-test")
  add_executable("Geometry-${target}" "${target}.cpp")
  target_link_libraries("Geometry-${target}" PRIVATE Catch2::Catch2WithMain)
endforeach()