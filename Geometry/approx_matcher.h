#include <catch2/catch_all.hpp>
#include "geometry.h"

template <geometry::floating_point T>
auto approx(T x) {
  return Catch::Matchers::WithinAbsMatcher(x, geometry::EPS<T>);
}