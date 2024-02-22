#include <catch2/catch_all.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>
#include "approx_matcher.h"
#include "geometry.h"

SCENARIO("Point") {
  GIVEN("a point with x and y coordinates") {
    auto x = 1.337;
    auto y = 8.848;

    Point p{x, y};

    THEN("comparison is correct") {
      Point q{x + 0.3 - 0.2 - 0.1, y + 0.3 - 0.2 - 0.1};
      Point r{x + .000001, y + .000001};
      REQUIRE(p == q);
      REQUIRE(p != r);
    }

    THEN("fields are accessible") {
      REQUIRE(p.x == x);
      REQUIRE(p.y == y);
    }

    THEN("rotation is correct") {
      WHEN("rotated around center") {
        CHECK(p.rotate(30.l) == Point{-3.266124035140205, 8.331092772684714});
      }
      WHEN("rotated around some point") {
        CHECK(p.rotate(Point{1, 1}, -90) == Point{8.848, 0.663});
      }
    }

    THEN("translation is correct") {
      Point q = {2, -3};
      CHECK(p.translate(q) == Point{x + q.x, y + q.y});
    }

    THEN("scaling is correct") {
      double factor = -1.488;
      CHECK(p.scale(factor) == Point{x * factor, y * factor});
    }

    THEN("reflection is correct") {
      WHEN("reflected around point") {
        CHECK(p.reflect(Point{3.5, 4}) == Point{5.663, -0.848});
      }
      WHEN("reflected around line") {
        Line line(Point{2, 3}, Point{5, 7});
        CHECK(p.reflect(line) == Point{7.79972, 4.00096});
        CHECK(Point{4, 2}.reflect(line) == Point{0.48, 4.64});
        Point q = {3.5, 5};  // lies on line
        CHECK(q.reflect(line) == q);
      }
    }
  }
}

SCENARIO("Line") {
  SECTION("distance is correct") {
    Line line(Point{2, 3}, Point{5, 7});
    CAPTURE(line);
    REQUIRE_THAT(line.distanceTo(Point{0, 0}), approx(0.2l));
    REQUIRE_THAT(line.distanceTo(Point{3, 7}), approx(1.6l));
  }

  GIVEN("a line through two points") {
    auto x1 = GENERATE(take(5, random(-100., 100.)));
    auto y1 = GENERATE(take(5, random(-100., 100.)));
    auto x2 = GENERATE(take(5, random(-100., 100.)));
    auto y2 = GENERATE(take(5, random(-100., 100.)));
    Point p1{x1, y1};
    Point p2{x2, y2};
    Line line(p1, p2);
    CAPTURE(line, p1, p2);

    THEN("both of them lie on it") {
      REQUIRE_THAT(line.distanceTo(p1), approx(0.l));
      REQUIRE_THAT(line.distanceTo(p2), approx(0.l));
    }

    THEN("it is equal to itself") {
      REQUIRE(line == line);
    }
  }

  GIVEN("a line by coefficients") {
    auto k = GENERATE(take(5, random(-100., 100.)));
    auto b = GENERATE(take(5, random(-100., 100.)));
    Line line(k, b);
    CAPTURE(line, k, b);

    THEN("corresponding points lie on it") {
      REQUIRE_THAT(line.distanceTo(Point(0, b)), approx(0.l));
      if (k != 0) {
        REQUIRE_THAT(line.distanceTo(Point(-b / k, 0)), approx(0.l));
      }
    }
  }

  GIVEN("a line by point and coefficient") {
    auto x1 = GENERATE(take(5, random(-100., 100.)));
    auto y1 = GENERATE(take(5, random(-100., 100.)));
    Point p(x1, y1);
    auto k = GENERATE(take(5, random(-100., 100.)));
    Line line(p, k);
    CAPTURE(line, p, k);

    THEN("point lies on it") {
      REQUIRE_THAT(line.distanceTo(p), approx(0.l));
    }

    THEN("it equals to two coefficients") {
      REQUIRE(line == Line(k, y1 - k * x1));
    }
  }
}
