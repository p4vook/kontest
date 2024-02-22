#include <catch2/catch_all.hpp>
#include "approx_matcher.h"
#include "geometry.h"

SCENARIO("Ellipse") {
  GIVEN("two foci and distance sum") {
    Point focus1{1.337, 8.848};
    Point focus2{-0.3, std::sqrt(3)};
    double distance_sum = 5.88 * 2;

    WHEN("an elipse is constructed") {
      Ellipse ell(focus1, focus2, distance_sum);

      THEN("foci are correct") {
        REQUIRE(ell.focuses() == std::pair{focus1, focus2});
      }

      THEN("directrixes are correct") {
        CAPTURE(ell.directrices().first, ell.directrices().second);
        REQUIRE(ell.directrices() ==
                std::pair{
                    Line(-0.224190835553884, -0.974545262803966, 14.741695987724055),
                    Line(-0.224190835553884, -0.974545262803966, -4.198471696513153),
                });
      }

      THEN("eccentricity is correct") {
        REQUIRE_THAT(ell.eccentricity(), approx(0.620902633812855));
      }

      THEN("center is correct") {
        REQUIRE(ell.center() == Point{0.5185, 5.290025403784439});
      }

      THEN("area is correct") {
        CAPTURE(ell.area());
        REQUIRE_THAT(ell.area(), approx(85.144847582115));
      }

      THEN("perimeter is correct") {
        CAPTURE(ell.perimeter());
        REQUIRE_THAT(ell.perimeter(), approx(33.074001391578776));
      }

      THEN("swapping focuses doesn't change ellipse") {
        REQUIRE(ell == Ellipse(focus2, focus1, distance_sum));
      }

      THEN("containment is correct") {
        REQUIRE(ell.containsPoint(Point(0, 10)));
        REQUIRE(ell.containsPoint(Point(-4, 6)));
        REQUIRE(ell.containsPoint(Point(1.836742113056817, 11.020351549071766)));

        REQUIRE(!ell.containsPoint(Point(2, 11)));
        REQUIRE(!ell.containsPoint(Point(-1, -0.5)));
      }

      THEN("equality is OK") {
        Polygon p(Point(1, 1), Point(2, 1));
        REQUIRE(*dynamic_cast<Shape*>(&ell) != *dynamic_cast<Shape*>(&p));
      }
    }
  }
}