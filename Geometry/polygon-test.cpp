#include <catch2/catch_all.hpp>
#include <iostream>
#include "approx_matcher.h"
#include "geometry.h"

SCENARIO("Polygon") {
  GIVEN("some points") {
    std::vector<Point> v = {Point(1, 2), Point(2, 3), Point(-0.5, -1), Point(-3, 1)};
    WHEN("a polygon is constructed") {
      Polygon poly(v);

      THEN("the points did not change") {
        REQUIRE(poly.verticesCount() == v.size());
        REQUIRE(poly.getVertices() == v);
      }

      Polygon poly_va(v[1], v[2], v[3], v[0]);
      THEN("polygon can be constructed variadically") {
        REQUIRE(poly == poly_va);
      }

      THEN("area is computed correctly") {
        REQUIRE_THAT(poly.area(), approx(6.));
      }

      THEN("perimeter is computed correctly") {
        REQUIRE_THAT(poly.perimeter(), approx(13.455871872735482));
      }

      THEN("containment is correct") {
        REQUIRE(Polygon(Point(0, 0), Point(2, 0)).containsPoint(Point(1, 0)));
        REQUIRE(poly.containsPoint(Point(0, 0)));
        REQUIRE(poly.containsPoint(Point(2, 3)));
        REQUIRE(poly.containsPoint(Point(-2, 1)));

        REQUIRE_FALSE(poly.containsPoint(Point(1, 0)));
        REQUIRE_FALSE(poly.containsPoint(Point(0, 2)));
      }

      THEN("shifted polygon is the same") {
        REQUIRE(poly == poly_va);
      }

      THEN("polygon in other direction is the same") {
        REQUIRE(poly == Polygon(v[3], v[2], v[1], v[0]));
      }

      THEN("rotation is correct") {
        Point center{0, 1};
        double angle = -45;
        Polygon rotated(v[0].rotate(center, angle), v[1].rotate(center, angle),
                        v[2].rotate(center, angle), v[3].rotate(center, angle));
        poly.rotate(Point(0, 1), -45);
        REQUIRE(poly == rotated);
      }

      THEN("reflection is correct") {
        GIVEN("a point") {
          Point p{3, 7};
          Polygon reflected =
              Polygon(v[0].reflect(p), v[1].reflect(p), v[2].reflect(p), v[3].reflect(p));
          poly.reflect(p);
          REQUIRE(poly == reflected);
        }

        GIVEN("a line") {
          Line l(Point{-1, 1}, Point{2, 3});
          Polygon reflected =
              Polygon(v[0].reflect(l), v[1].reflect(l), v[2].reflect(l), v[3].reflect(l));
          poly.reflect(l);
          REQUIRE(poly == reflected);
        }
      }

      THEN("scaling is correct") {
        Point center{3, 1};
        double scale = 2.5;

        Polygon scaled(v[0].scale(center, scale), v[1].scale(center, scale),
                       v[2].scale(center, scale), v[3].scale(center, scale));
        poly.scale(center, scale);
        REQUIRE(poly == scaled);
      }

      double shiftX = 1.5;
      double shiftY = 2.7;
      auto transformed =
          Polygon(Point(v[1].x + shiftX, v[1].y + shiftY), Point(v[2].x + shiftX, v[2].y + shiftY),
                  Point(v[3].x + shiftX, v[3].y + shiftY), Point(v[0].x + shiftX, v[0].y + shiftY));

      transformed.rotate(Point(-1, 2), 37);
      transformed.reflect(Line(1, 1));

      THEN("congruency is correct") {
        REQUIRE(poly.isCongruentTo(transformed));
        REQUIRE(transformed.isCongruentTo(poly));
      }

      THEN("similarity is correct") {
        transformed.scale(Point(3, 1), -1.5);
        REQUIRE(poly.isSimilarTo(transformed));
        REQUIRE(transformed.isSimilarTo(poly));
      }
    }
  }

  GIVEN("a non-convex polygon") {
    Polygon poly(Point(0, 0), Point(1, 1), Point(2, 1), Point(0, 2));

    THEN("it is indeed non-convex") {
      REQUIRE_FALSE(poly.isConvex());
    }
  }

  GIVEN("a convex polygon") {
    Polygon poly(Point(1, 4), Point(1, 1), Point(2, 3), Point(4, 8));

    THEN("it is indeed convex") {
      REQUIRE(poly.isConvex());
    }
  }

  GIVEN("a symmetric polygon") {
    Polygon poly1(Point{0, 0}, Point{1, 0}, Point{1, 1}, Point{0, 1});
    Polygon poly2(Point{3, 4}, Point{3, 5}, Point{2, 5}, Point{2, 4});
    REQUIRE(poly1.isCongruentTo(poly2));
    REQUIRE(poly2.isCongruentTo(poly1));
  }
}

SCENARIO("Rectangle") {
  GIVEN("two points and a ratio") {
    Point a{1.5, 2};
    Point b{1, 1};
    Rectangle rect(a, b, std::sqrt(3));
    CAPTURE(rect);
    THEN("rectangle aligns correctly") {
      REQUIRE(rect ==
              Polygon(a, Point(1.80801270189, 1.53349364905), b, Point(0.691987298, 1.4665063509)));
    }
    THEN("rectangle area is correct") {
      REQUIRE_THAT(rect.area(), approx(0.5412658773653));
    }
    THEN("rectangle center is correct") {
      REQUIRE(rect.center() == Point(1.25, 1.5));
    }
    THEN("rectangle diagonals are correct") {
      CAPTURE(rect.diagonals().first, rect.diagonals().second);

      REQUIRE(rect.diagonals() ==
              std::pair{Line(a, b), Line(-0.0669872981078, 1.1160254037844, -1.5903039830419)});
    }
  }
}

SCENARIO("Square") {
  GIVEN("two points as opposite vertices") {
    Square square(Point(1.337, 8.848), Point(-0.966, 2.329));

    THEN("square is correct") {
      REQUIRE(square == Polygon(Point(1.337, 8.848), Point(3.445, 4.437), Point(-0.966, 2.329),
                                Point(-3.074, 6.74)));
    }

    THEN("inscribed circle is correct") {
      CAPTURE(square.inscribedCircle());
    }

    THEN("circumscribed circle is correct") {
      CAPTURE(square.circumscribedCircle());
    }
  }
}

SCENARIO("Triangle") {
  GIVEN("some three points") {
    Point a{1, 2};
    Point b{4, 7};
    Point c{-5, 2};
    Triangle triangle(a, b, c);

    THEN("circumcircle") {
      const auto v = triangle.circumscribedCircle();
      REQUIRE(v == Circle(Point(-2, 7.2), 6.003332407921453));
    }

    THEN("incircle") {
      const auto v = triangle.inscribedCircle();
      REQUIRE(v == Circle(Point(0.232339123070849, 3.355835255143217), 1.355835255143217));
    }

    THEN("orthocenter") {
      const auto v = triangle.orthocenter();
      REQUIRE(v == Point{4, -3.4});
    }

    THEN("Euler line") {
      const auto v = triangle.EulerLine();
      REQUIRE(v == Line(10.6, 6, -22));
    }

    THEN("nine points circle") {
      const auto v = triangle.ninePointsCircle();
      REQUIRE(triangle == triangle);
      REQUIRE(v == Circle(Point(1, 1.9), 3.001666203960727));
    }

    THEN("equality") {
      REQUIRE(triangle == Polygon(a, b, c));
      REQUIRE(triangle != Polygon(a, b, -c));
    }
  }
}