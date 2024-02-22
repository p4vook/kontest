#include <catch2/catch_all.hpp>

#include <fstream>
#include <iostream>
#include <sstream>
#include "matrix.h"

TEST_CASE("matrix smol") {
  using M = Matrix<3, 3, Rational>;
  M x = {{1, 2, 3}, {2, 3, 4}, {3, 4, 5}};
  M y;
  REQUIRE(x + y + y + y + y == x);
  REQUIRE(x * Math::UnaryMatrix<3, 3, Rational>() == x);
  //  x.gaussianEliminate();
  REQUIRE(x.rank() == 2);
  REQUIRE(x.det() == 0);
  REQUIRE(x.transposed() == x);
  REQUIRE((x * y) * Rational(2) == y);
  REQUIRE(x * Rational(2) == M{{2, 4, 6}, {4, 6, 8}, {6, 8, 10}});
  REQUIRE(Rational(2) * x == M{{2, 4, 6}, {4, 6, 8}, {6, 8, 10}});
  y *= 3;
}

TEST_CASE("matrix kek") {
  using M = Matrix<3, 3, Residue<7>>;
  M x = {{1, 2, 1}, {2, 3, 4}, {3, 4, 5}};
  REQUIRE(x.inverted() * x == Math::UnaryMatrix<3, 3, Residue<7>>());
  REQUIRE(x * x.inverted() == Math::UnaryMatrix<3, 3, Residue<7>>());
}

TEST_CASE("matrix kok") {
  std::istringstream ss("-1/3");
  using M = Matrix<3, 3>;
  M x;
  ss >> x[0, 1];
  REQUIRE(x == M({{0, Rational(-1, 3), 0}, {0, 0, 0}, {0, 0, 0}}));
}

TEST_CASE("matrix kik") {
  std::ifstream in("matr.txt");
  const size_t N = 5;
  Matrix<N, N, Rational> kok;
  for (unsigned i = 0; i < N; ++i) {
    for (unsigned j = 0; j < N; ++j) {
      in >> kok[i, j];
    }
  }
//  REQUIRE(kok * kok.inverted() == Math::UnaryMatrix<N, N, Rational>());
}

TEST_CASE("matrix aboba") {
  STATIC_REQUIRE(Math::Concepts::Ring<Matrix<3, 3, Rational>>);
  STATIC_REQUIRE(Math::Concepts::Field<Matrix<3, 3, Rational>>);
  using M = SquareMatrix<3, Rational>;
  using MM = SquareMatrix<2, M>;
  using MMM = SquareMatrix<2, MM>;
  MMM x, y, z;
  std::cerr << x + x * x << std::endl;
  //  REQUIRE(MM().inverted() == MM());
}

struct kek {
    int t;

    kek() : t(0) {
    }

    kek(int x) : t(x) {
    }

    kek operator+(const kek&) const {
      return *this;
    }

    kek operator*(const kek&) const {
      return *this;
    }

    kek operator-(const kek&) const {
        return *this;
    }
};

TEST_CASE("matrix boboba") {
  STATIC_REQUIRE(Math::Concepts::Ring<kek>);
  Matrix<3, 3, kek> aboba;
}
