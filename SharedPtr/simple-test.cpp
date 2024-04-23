#include <catch2/catch_all.hpp>
#include "shared_ptr.h"

TEST_CASE("simple test") {
  auto kek = makeShared<int>(10);
  REQUIRE(*kek == 10);
  auto kok = kek;
  auto kik = std::move(kek);
  REQUIRE(*kok == 10);
  REQUIRE(*kik == 10);
}

TEST_CASE("struct test") {
  struct S {
    int x;
    int y;
  };

  SECTION("simple") {
    auto kek = makeShared<S>(10, 15);
    auto kok = kek;
    auto kik = std::move(kek);
    REQUIRE(kok->x == 10);
    REQUIRE(kik->x == 10);
  }

  SECTION("substruct") {
    struct T : S {};
    T* struct_ptr = new T{{10, 15}};
    SharedPtr<T> sub_ptr(struct_ptr);
    SharedPtr<S> ptr = sub_ptr;
  }
}