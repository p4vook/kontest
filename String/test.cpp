#include <catch2/catch_test_macros.hpp>
#include <sstream>
#include "string.h"

TEST_CASE() {
  SECTION("String outputs \\0") {
    String str(1, '\0');
    std::ostringstream os;
    os << str;
    REQUIRE(os.str().size() == 1);
  }
}
