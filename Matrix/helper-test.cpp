#include "matrix.h"
#include <catch2/catch_all.hpp>

TEST_CASE("Prime check") {
  STATIC_REQUIRE(Math::isPrime(37));
  STATIC_REQUIRE(Math::isPrime(5));
  STATIC_REQUIRE_FALSE(Math::isPrime(1));
  STATIC_REQUIRE_FALSE(Math::isPrime(15));
}

template <std::integral T>
std::tuple<T, T, T> extendedGCD(T a, T b) {
  if (b == 0) {
    return {1, 0, a};
  }
  auto [x, y, d] = extendedGCD(b, a % b);
  return {y, x - y * (a / b), d};
}

TEST_CASE("extended gcd") {
  SECTION("gcd 1") {
    auto [x, y, d] = extendedGCD(-5, 7);
    CAPTURE(x, y, d);
    REQUIRE(abs(d) == 1);
    REQUIRE(x * (-5) + y * 7 == d);
  }

  SECTION("gcd not 1") {
    auto [x, y, d] = extendedGCD(12, -30);
    REQUIRE(abs(d) == 6);
    REQUIRE(x * 12 - y * 30 == d);
  }
}

SCENARIO("Residue") {
  GIVEN("non-prime modulo") {
    using R = Residue<12>;

    THEN("cast from/to int works") {
      REQUIRE(static_cast<int>(R(5)) == 5);
      REQUIRE(static_cast<int>(R(103)) == 7);
      REQUIRE(static_cast<int>(R(-5)) == 7);
      REQUIRE(static_cast<int>(R(-20)) == 4);
    }

    THEN("arithmetics work") {
      REQUIRE(R(5) + R(7) == R(0));
      REQUIRE(R(5) * R(7) == R(11));
      REQUIRE(R(8) * R(5) == R(4));
      REQUIRE(R(20) - R(57) == R(11));
      REQUIRE(-R(0) == R(0));
      REQUIRE(-R(6) == R(6));
    }
  }

  GIVEN("prime modulo") {
    using R = Residue<7>;

    THEN("division works") {
      REQUIRE(R(3) / R(5) == R(2));
      REQUIRE(R(1) / R(6) == R(6));
    }
  }
}