#include <catch2/catch_all.hpp>
#include <iostream>
#include "biginteger.h"

TEST_CASE("Plus OK", "[correctness]") {
  auto x = GENERATE(take(1000, random(-1000, 1000)));
  auto y = GENERATE(take(1000, random(-1000, 1000)));
  REQUIRE(x + y == BigInteger(x) + BigInteger(y));
  REQUIRE(x - y == BigInteger(x) - BigInteger(y));
}

TEST_CASE("Multiply OK", "[correctness]") {
  auto x = GENERATE(take(1000, random(-1000, 1000)));
  auto y = GENERATE(take(1000, random(-1000, 1000)));
  REQUIRE(x * y == BigInteger(x) * BigInteger(y));
}

TEST_CASE("Divide OK", "[correctness]") {
  auto x = GENERATE(take(1000, random(-1000, 1000)));
  auto y = GENERATE(take(1000, filter([](int x) { return x != 0; }, random(-1000, 1000))));
  REQUIRE(x / y == BigInteger(x) / BigInteger(y));
  REQUIRE(x % y == BigInteger(x) % BigInteger(y));
}

TEST_CASE("benchmark", "[!benchmark]") {
  BENCHMARK("Multiply bench") {
    BigInteger current = 2;
    for (int i = 1; i <= 15; ++i) {
      current *= current;
    }
    return current;
  };

  BigInteger temp(2);
  for (int i = 1; i <= 15; ++i) {
    temp *= temp;
  }

  BENCHMARK("toString bench") {
    return temp.toString();
  };

  temp = 1;

  BENCHMARK("Add bench") {
    for (int i = 1; i <= 1000; ++i) {
      temp += temp;
    }
    return temp;
  };
}