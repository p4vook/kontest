#include "matrix.h"
#include <iostream>
#include <fstream>
#include <cassert>

int main() {
  char start;
  std::cin >> start;
  std::ifstream in("matr.txt");
  const size_t N = 20;
  Matrix<N, N, Rational> kok;
  for (unsigned i = 0; i < N; ++i) {
    for (unsigned j = 0; j < N; ++j) {
      in >> kok[i, j];
    }
  }
  assert((kok * kok.inverted() == Matrix<N, N, Rational>::unityMatrix()));
}
