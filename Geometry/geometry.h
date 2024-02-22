#ifndef GEOMETRY_H
#define GEOMETRY_H

#include <algorithm>
#include <cfloat>
#include <cmath>
#include <cstdlib>
#include <ostream>
#include <vector>

namespace geometry {
template <typename T>
concept floating_point = std::is_floating_point_v<T>;

template <typename T>
const static T EPS = 0;

template <>
const static float EPS<float> = std::sqrt(FLT_EPSILON);

template <>
const static double EPS<double> = std::sqrt(DBL_EPSILON);

template <>
const static long double EPS<long double> = std::sqrt(LDBL_EPSILON);

template <floating_point T>
constexpr bool isZero(T value) {
  return std::abs(value) < EPS<T>;
}

template <floating_point T>
constexpr bool areEqual(T a, T b) {
  return isZero(a - b);
}

template <floating_point T>
constexpr T degreesToRadians(T degrees_value) {
  return degrees_value * M_PIl / 180;
}

template <floating_point T>
constexpr T radiansToDegrees(T radians_value) {
  return radians_value * M_1_PIl * 180;
}

class Line;

template <floating_point value_type>
struct Point {
    value_type x;
    value_type y;

    constexpr Point() : x(value_type()), y(value_type()) {
    }

    constexpr Point(value_type x_, value_type y_) : x(x_), y(y_) {
    }

    template <floating_point coordinate_type>
    constexpr explicit Point(const Point<coordinate_type>& point)
        : x(static_cast<value_type>(point.x)), y(static_cast<value_type>(point.y)) {
    }

    constexpr Point operator+=(const Point& other) {
      x += other.x;
      y += other.y;
      return *this;
    }

    constexpr Point operator-() const {
      return Point{-x, -y};
    }

    constexpr Point operator-=(const Point& other) {
      *this += -other;
      return *this;
    }

    constexpr Point rotate90() const {
      return Point{-y, x};
    }

    constexpr Point rotate(value_type angle_degrees) const {
      value_type angle_radians = degreesToRadians(angle_degrees);
      return Point{x * std::cos(angle_radians) - y * std::sin(angle_radians),
                   x * std::sin(angle_radians) + y * std::cos(angle_radians)};
    }

    constexpr Point rotate(const Point& center, value_type angle_degrees) const {
      return center + (center >> *this).rotate(angle_degrees);
    }

    constexpr Point reflect(const Point& other) const {
      return {2 * other.x - x, 2 * other.y - y};
    }

    constexpr Point reflect(const Line& line) const;

    constexpr Point translate(const Point& other) const {
      return *this + other;
    }

    constexpr Point scale(value_type coefficient) const {
      return Point{x * coefficient, y * coefficient};
    }

    constexpr Point scale(const Point& center, value_type coefficient) const {
      return center + (center >> *this).scale(coefficient);
    }

    template <floating_point result_type = value_type>
    constexpr result_type length() const;

    constexpr Point normalize() const {
      return scale(static_cast<value_type>(1 / length<long double>()));
    }
};

template <floating_point coordinate_type>
std::ostream& operator<<(std::ostream& out, const Point<coordinate_type>& point) {
  typename std::ostream::sentry sentry(out);
  if (sentry) {
    out << "(" << point.x << ", " << point.y << ")";
  }
  return out;
}

template <floating_point coordinate_type>
constexpr bool operator==(const Point<coordinate_type>& lhs, const Point<coordinate_type>& rhs) {
  return areEqual(lhs.x, rhs.x) && areEqual(lhs.y, rhs.y);
}

template <floating_point coordinate_type>
constexpr Point<coordinate_type> operator+(const Point<coordinate_type>& lhs,
                                           const Point<coordinate_type>& rhs) {
  Point copy = lhs;
  return copy += rhs;
}

template <floating_point coordinate_type>
constexpr Point<coordinate_type> operator-(const Point<coordinate_type>& lhs,
                                           const Point<coordinate_type>& rhs) {
  Point copy = lhs;
  return copy -= rhs;
}

template <floating_point coordinate_type>
constexpr Point<coordinate_type> operator>>(const Point<coordinate_type>& lhs,
                                            const Point<coordinate_type>& rhs) {
  return rhs - lhs;
}

template <floating_point coordinate_type_1,
          floating_point coordinate_type_2,
          floating_point result_type = long double>
constexpr result_type dot(const Point<coordinate_type_1>& lhs,
                          const Point<coordinate_type_2>& rhs) {
  return static_cast<result_type>(lhs.x) * rhs.x + static_cast<result_type>(lhs.y) * rhs.y;
}

template <floating_point coordinate_type_1,
          floating_point coordinate_type_2,
          floating_point result_type = long double>
constexpr result_type cross(const Point<coordinate_type_1>& lhs,
                            const Point<coordinate_type_2>& rhs) {
  return static_cast<result_type>(lhs.x) * rhs.y - static_cast<result_type>(lhs.y) * rhs.x;
}

template <floating_point coordinate_type_1,
          floating_point coordinate_type_2,
          floating_point result_type = long double>
constexpr result_type angle(const Point<coordinate_type_1>& lhs,
                            const Point<coordinate_type_2>& rhs) {
  return std::atan2(cross<coordinate_type_1, coordinate_type_2, result_type>(lhs, rhs),
                    dot<coordinate_type_1, coordinate_type_2, result_type>(lhs, rhs));
}

template <floating_point value_type>
template <floating_point result_type>
constexpr result_type Point<value_type>::length() const {
  return std::sqrt(dot<value_type, value_type, result_type>(*this, *this));
}

class Line {
  private:
    long double a;
    long double b;
    long double c;

  public:
    constexpr Line(long double a_, long double b_, long double c_) : a(a_), b(b_), c(c_) {
    }

    template <floating_point coordinate_type>
    constexpr Line(const Point<coordinate_type>& point1, const Point<coordinate_type>& point2)
        : c(cross(point1, point2)) {
      Point normal = (point2 - point1).rotate90();
      a = normal.x;
      b = normal.y;
    }

    constexpr Line(long double coefficient, long double offset) : a(coefficient), b(-1), c(offset) {
    }

    template <floating_point coordinate_type>
    constexpr Line(const Point<coordinate_type>& point, long double coefficient)
        : a(coefficient), b(-1), c(-a * point.x - b * point.y) {
    }

    constexpr Point<long double> normal() const {
      return Point{a, b};
    }

    friend constexpr bool operator==(const Line& lhs, const Line& rhs);

    template <floating_point coordinate_type>
    constexpr long double operator()(const Point<coordinate_type>& point) const {
      return dot(normal(), point) + c;
    }

    template <floating_point coordinate_type>
    constexpr long double distanceTo(const Point<coordinate_type>& point) const {
      return std::abs((*this)(point)) / normal().length<long double>();
    }

    friend std::ostream& operator<<(std::ostream& out, const Line& line);
};

template <floating_point coordinate_type>
constexpr Point<coordinate_type> Point<coordinate_type>::reflect(const Line& line) const {
  const Point<coordinate_type> SCALED_NORMAL(
      line.normal().normalize().scale(2 * line.distanceTo(*this)));
  return (line(*this) > 0 ? *this - SCALED_NORMAL : *this + SCALED_NORMAL);
}

constexpr bool operator==(const Line& lhs, const Line& rhs) {
  return isZero(lhs.a * rhs.b - lhs.b * rhs.a) && isZero(lhs.a * rhs.c - lhs.c * rhs.a) &&
         isZero(lhs.b * rhs.c - lhs.c * rhs.b);
}

std::ostream& operator<<(std::ostream& out, const Line& line) {
  typename std::ostream::sentry sentry(out);
  if (sentry) {
    out << "{" << line.a << "x + " << line.b << "y + " << line.c << " = 0}";
  }
  return out;
}

template <floating_point coordinate_type>
class Shape {
  public:
    constexpr virtual coordinate_type area() const = 0;
    constexpr virtual coordinate_type perimeter() const = 0;
    constexpr virtual bool containsPoint(const Point<coordinate_type>& point) const = 0;

    constexpr virtual bool operator==(const Shape<coordinate_type>& other) const& = 0;

    constexpr virtual bool isCongruentTo(const Shape& other) const& = 0;

    constexpr virtual bool isSimilarTo(const Shape& other) const& = 0;

    constexpr virtual void rotate(const Point<coordinate_type>& center, coordinate_type angle) = 0;

    constexpr virtual void translate(const Point<coordinate_type>& shift) = 0;

    constexpr virtual void scale(const Point<coordinate_type>& center, coordinate_type scale) = 0;

    constexpr virtual void reflect(const Point<coordinate_type>& center) = 0;

    constexpr virtual void reflect(const Line& line) = 0;

    constexpr Shape() = default;
    constexpr Shape(const Shape& other) = default;
    constexpr Shape(Shape&& other) = default;
    constexpr Shape& operator=(const Shape& other) = default;
    constexpr Shape& operator=(Shape&& other) = default;
    constexpr virtual ~Shape() = default;
};

template <floating_point coordinate_type>
class Polygon : public Shape<coordinate_type> {
  private:
    std::vector<Point<coordinate_type>> vertices;

  public:
    Polygon(const std::vector<Point<coordinate_type>>& points) : vertices(points) {
    }

    template <typename... args>
    Polygon(args... points) : vertices{{points...}} {
    }

    constexpr size_t verticesCount() const {
      return vertices.size();
    }

    constexpr const std::vector<Point<coordinate_type>>& getVertices() const {
      return vertices;
    }

    constexpr auto rend() const {
      return vertices.rend();
    }

    // returns vertex
    constexpr const Point<coordinate_type>& operator[](size_t index) const {
      return getVertices()[index % verticesCount()];
    }

    // returns side (i, i + 1)
    constexpr Point<coordinate_type> operator()(size_t index) const {
      return (*this)[index] >> (*this)[index + 1];
    }

    // returns angle
    constexpr coordinate_type operator^(size_t index) const {
      const auto SIDE_A = -(*this)(index > 0 ? index - 1 : verticesCount() - 1);
      const auto SIDE_B = (*this)(index);
      return static_cast<coordinate_type>(angle(SIDE_A, SIDE_B));
    }

    constexpr Point<coordinate_type> center0() const {
      Point<coordinate_type> result;
      for (const auto& vertex : vertices) {
        result += vertex;
      }
      return result.scale(1 / static_cast<coordinate_type>(verticesCount()));
    }

    constexpr Point<coordinate_type> center1() const {
      Point<coordinate_type> result;
      for (unsigned i = 0; i < vertices.size(); ++i) {
        result += ((*this)[i] + (*this)[i + 1]).scale((*this)(i).length());
      }
      return result.scale(1 / (2 * perimeter()));
    }

    constexpr bool operator==(const Shape<coordinate_type>& other) const& override {
      auto polygon = dynamic_cast<const Polygon<coordinate_type>*>(&other);
      return polygon && *this == *polygon;
    }

    constexpr coordinate_type area() const override {
      coordinate_type result = 0;  // TODO Rump-Ogita-Oishi // WONTFIX
      for (unsigned i = 0; i < vertices.size(); ++i) {
        result += static_cast<coordinate_type>(cross((*this)[i], (*this)[i + 1]));
      }
      return std::abs(result) / 2;
    }

    constexpr coordinate_type perimeter() const override {
      coordinate_type result = 0;  // TODO Rump-Ogita-Oishi
      for (unsigned i = 0; i < vertices.size(); ++i) {
        result += static_cast<coordinate_type>((*this)(i).length());
      }
      return result;
    }

    constexpr bool containsPoint(const Point<coordinate_type>& point) const override {
      if (vertices.size() == 0) {
        return false;
      }
      if (vertices.size() == 1) {
        return point == vertices[0];
      }
      long double angle_sum = 0;
      for (unsigned i = 0; i < vertices.size(); ++i) {
        auto lhs = point >> (*this)[i];
        auto rhs = point >> (*this)[i + 1];
        if (areEqual(cross(lhs, rhs), 0.l) && dot(lhs, lhs - rhs) > -geometry::EPS<long double>) {
          return true;
        }
        angle_sum += static_cast<coordinate_type>(angle(lhs, rhs));
      }
      return areEqual(std::abs(angle_sum), 2 * static_cast<long double>(M_PIl));
    }

    constexpr void rotate(const Point<coordinate_type>& center, coordinate_type angle) override {
      for (auto& vertex : vertices) {
        vertex = vertex.rotate(center, angle);
      }
    }

    constexpr void reflect(const Point<coordinate_type>& center) override {
      for (auto& vertex : vertices) {
        vertex = vertex.reflect(center);
      }
    }

    constexpr void reflect(const Line& line) override {
      for (auto& vertex : vertices) {
        vertex = vertex.reflect(line);
      }
    }

    constexpr void scale(const Point<coordinate_type>& center, coordinate_type scale) override {
      for (auto& vertex : vertices) {
        vertex = vertex.scale(center, scale);
      }
    }

    constexpr void translate(const Point<coordinate_type>& shift) override {
      for (auto& vertex : vertices) {
        vertex = vertex.translate(shift);
      }
    }

    constexpr bool isConvex() const {
      bool last = false;
      for (unsigned i = 0; i < vertices.size(); ++i) {
        bool current = cross((*this)[i] >> (*this)[i + 1], (*this)[i] >> (*this)[i + 2]) > 0;
        if (i > 0 && current != last) {
          return false;
        }
        last = current;
      }
      return true;
    }

    template <bool allow_scale>
    constexpr bool matchByPointPair(
        const Polygon<coordinate_type>& other,
        std::pair<Point<coordinate_type>, Point<coordinate_type>> my_inv,
        std::pair<Point<coordinate_type>, Point<coordinate_type>> other_inv) const& {
      Polygon copy(other);
      if (my_inv.first != other_inv.first) {
        auto shift = other_inv.first >> my_inv.first;
        copy.translate(shift);
        other_inv.first = other_inv.first.translate(shift);
        other_inv.second = other_inv.second.translate(shift);
      }
      auto my_delta = my_inv.first >> my_inv.second;
      auto other_delta = other_inv.first >> other_inv.second;
      if (!areEqual(my_delta.length(), other_delta.length())) {
        if (!allow_scale) {
          return false;
        }
        coordinate_type scale = my_delta.length() / other_delta.length();
        copy.scale(my_inv.first, scale);
      }
      if (my_delta != Point<coordinate_type>()) {
        auto delta_angle = static_cast<coordinate_type>(angle(my_delta, other_delta));
        copy.rotate(my_inv.first, -radiansToDegrees(delta_angle));
      }
      if (*this == copy) {
        return true;
      }
      copy.reflect(Line(my_inv.first, my_inv.second));
      return *this == copy;
    }

    template <bool allow_scale>
    constexpr bool isEquivalentTo(const Polygon<coordinate_type>& other) const& {
      if (verticesCount() != other.verticesCount()) {
        return false;
      }
      if (verticesCount() <= 1) {
        return true;
      }
      auto my_center0 = center0();
      auto my_center1 = center1();
      auto other_center0 = other.center0();
      auto other_center1 = other.center1();
      bool my_equal = my_center0 == my_center1;
      bool other_equal = other_center0 == other_center1;
      if (my_equal != other_equal) {
        return false;
      }
      if (!my_equal) {
        // proceed to O(n) algorithm
        return matchByPointPair<allow_scale>(other, std::pair{my_center0, my_center1},
                                             std::pair{other_center0, other_center1});
      } else {
        // too symmetric, resort to O(n^2) algorithm
        std::pair my_inv{(*this)[0], (*this)[1]};
        for (unsigned first_point = 0; first_point < other.verticesCount(); ++first_point) {
          if (matchByPointPair<allow_scale>(
                  other, my_inv, std::pair{other[first_point], other[first_point + 1]})) {
            return true;
          }
        }
        return false;
      }
    }

    constexpr bool isCongruentTo(const Shape<coordinate_type>& other) const& override {
      auto polygon = dynamic_cast<const Polygon<coordinate_type>*>(&other);
      return polygon && isEquivalentTo<false>(*polygon);
    }

    constexpr bool isSimilarTo(const Shape<coordinate_type>& other) const& override {
      auto polygon = dynamic_cast<const Polygon<coordinate_type>*>(&other);
      return polygon && isEquivalentTo<true>(*polygon);
    }
};

template <floating_point coordinate_type>
constexpr bool operator==(const Polygon<coordinate_type>& lhs,
                          const Polygon<coordinate_type>& rhs) {
  const auto& lhs_v = lhs.getVertices();
  const auto& rhs_v = rhs.getVertices();
  if (lhs_v.size() != rhs_v.size()) {
    return false;
  }
  if (rhs_v.empty()) {
    return true;
  }
  auto lhs_start = std::find(lhs_v.begin(), lhs_v.end(), rhs_v[0]);
  if (std::equal(lhs_start, lhs_v.end(), rhs_v.begin()) &&
      std::equal(lhs_v.begin(), lhs_start, rhs_v.begin() + (lhs_v.end() - lhs_start))) {
    return true;
  }
  auto lhs_start_inverted = std::find(lhs_v.rbegin(), lhs_v.rend(), rhs_v[0]);
  auto rhs_mid = rhs_v.begin() + (lhs_v.rend() - lhs_start_inverted);
  return std::equal(lhs_start_inverted, lhs_v.rend(), rhs_v.begin()) &&
         std::equal(lhs_v.rbegin(), lhs_start_inverted, rhs_mid);
}

template <floating_point coordinate_type>
std::ostream& operator<<(std::ostream& out, const Polygon<coordinate_type>& polygon) {
  typename std::ostream::sentry sentry(out);
  if (sentry) {
    for (const auto& vertex : polygon.getVertices()) {
      out << vertex << " ";
    }
  }
  return out;
}

template <floating_point coordinate_type>
class Rectangle : public Polygon<coordinate_type> {
  private:
    constexpr static long double getAngle(coordinate_type ratio) {
      return std::atan(std::max(ratio, 1 / ratio));
    }

    constexpr static Point<coordinate_type> getShift(const Point<coordinate_type>& main_diagonal,
                                                     coordinate_type ratio) {
      long double angle = getAngle(ratio);
      return main_diagonal.rotate(radiansToDegrees(angle)).scale(std::cos(angle));
    }

  public:
    Rectangle(const Point<coordinate_type>& a,
              const Point<coordinate_type>& b,
              coordinate_type ratio)
        : Polygon<coordinate_type>(a, a + getShift(a >> b, ratio), b, b - getShift(a >> b, ratio)) {
    }

    constexpr Point<coordinate_type> center() const {
      return Polygon<coordinate_type>::center0();
    }

    constexpr std::pair<Line, Line> diagonals() const {
      return {Line((*this)[0], (*this)[2]), Line((*this)[1], (*this)[3])};
    }
};

template <floating_point coordinate_type>
class Ellipse : public Shape<coordinate_type> {
  private:
    Polygon<coordinate_type> foci;
    coordinate_type distanceSum;

  public:
    Ellipse(Point<coordinate_type> focus1,
            Point<coordinate_type> focus2,
            coordinate_type distance_sum)
        : foci(focus1, focus2), distanceSum(distance_sum) {
    }

    constexpr std::pair<Point<coordinate_type>, Point<coordinate_type>> focuses() const {
      return {foci[0], foci[1]};
    }

    constexpr Point<coordinate_type> focusDelta() const {
      return foci[0] >> foci[1];
    }

    constexpr coordinate_type majorAxis() const {
      return distanceSum / 2;
    }

    constexpr coordinate_type minorAxis() const {
      const auto FOCUS_DELTA = focusDelta();
      return static_cast<coordinate_type>(std::sqrt(
          static_cast<long double>(majorAxis()) * majorAxis() - dot(FOCUS_DELTA, FOCUS_DELTA) / 4));
    }

    constexpr coordinate_type eccentricity() const {
      const long double K = minorAxis() / majorAxis();
      return static_cast<coordinate_type>(std::sqrt(1 - K * K));
    }

    constexpr Point<coordinate_type> center() const {
      return foci.center0();
    }

    constexpr std::pair<Line, Line> directrices() const {
      const auto DELTA = focusDelta().normalize().scale(majorAxis() / eccentricity());
      const auto D1 = center() - DELTA;
      const auto D2 = center() + DELTA;
      return {Line(D1, D1 + DELTA.rotate90()), Line(D2, D2 + DELTA.rotate90())};
    }

    constexpr bool operator==(const Ellipse& other) const& {
      return areEqual(distanceSum, other.distanceSum) && foci == other.foci;
    }

    constexpr bool operator==(const Shape<coordinate_type>& other) const& override {
      auto ellipse = dynamic_cast<const Ellipse<coordinate_type>*>(&other);
      return ellipse && *this == *ellipse;
    }

    constexpr coordinate_type area() const override {
      return M_PIl * majorAxis() * minorAxis();
    }

    constexpr coordinate_type perimeter() const override {
      return 4 * majorAxis() * std::comp_ellint_2(eccentricity());
    }

    constexpr void translate(const Point<coordinate_type>& shift) override {
      foci.translate(shift);
    }

    constexpr void rotate(const Point<coordinate_type>& center, coordinate_type degree) override {
      foci.rotate(center, degree);
    }

    constexpr void scale(const Point<coordinate_type>& center, coordinate_type scale) override {
      foci.scale(center, scale);
      distanceSum *= std::abs(scale);
    }

    constexpr void reflect(const Point<coordinate_type>& center) override {
      foci.reflect(center);
    }

    constexpr void reflect(const Line& line) override {
      foci.reflect(line);
    }

    constexpr bool isCongruentTo(const Ellipse<coordinate_type>& other) const& {
      return areEqual(distanceSum, other.distanceSum) && foci.isCongruentTo(other);
    }

    constexpr bool isCongruentTo(const Shape<coordinate_type>& other) const& override {
      auto ellipse = dynamic_cast<const Ellipse<coordinate_type>*>(&other);
      return ellipse && isCongruentTo(*ellipse);
    }

    constexpr bool isSimilarTo(const Ellipse<coordinate_type>& other) const& {
      return foci.isSimilarTo(other.foci) && (isZero(distanceSum) == isZero(other.distanceSum));
    }

    constexpr bool isSimilarTo(const Shape<coordinate_type>& other) const& override {
      auto ellipse = dynamic_cast<const Ellipse<coordinate_type>*>(&other);
      return ellipse && isSimilarTo(other);
    }

    constexpr bool containsPoint(const Point<coordinate_type>& point) const override {
      return (point >> foci[0]).length() + (point >> foci[1]).length() <
             distanceSum + geometry::EPS<coordinate_type>;
    }
};

template <floating_point coordinate_type>
class Circle : public Ellipse<coordinate_type> {
  public:
    Circle(const Point<coordinate_type>& center, coordinate_type radius)
        : Ellipse<coordinate_type>(center, center, 2 * radius) {
    }

    constexpr coordinate_type radius() const {
      return Ellipse<coordinate_type>::majorAxis();
    }
};

template <floating_point coordinate_type>
class Square : public Rectangle<coordinate_type> {
  public:
    Square(const Point<coordinate_type>& a, const Point<coordinate_type>& b)
        : Rectangle<coordinate_type>(a, b, static_cast<coordinate_type>(1)) {
    }

    Circle<coordinate_type> circumscribedCircle() const {
      const auto center = this->center();
      return Circle<coordinate_type>(center, (center >> (*this)[0]).length());
    }

    Circle<coordinate_type> inscribedCircle() const {
      const auto center = this->center();
      return Circle<coordinate_type>(center, (*this)(0).length());
    }
};

template <floating_point coordinate_type>
class Triangle : public Polygon<coordinate_type> {
  public:
    Triangle(const Point<coordinate_type>& a,
             const Point<coordinate_type>& b,
             const Point<coordinate_type>& c)
        : Polygon<coordinate_type>(a, b, c) {
    }

    constexpr Point<coordinate_type> centroid() const {
      return Polygon<coordinate_type>::center0();
    }

    constexpr Point<coordinate_type> orthocenter() const {
      Point<coordinate_type> result{0, 0};
      coordinate_type tan_sum = 0;
      for (size_t i = 0; i < 3; ++i) {
        coordinate_type current_tan = std::tan(*this ^ i);
        result += (*this)[i].scale(current_tan);
        tan_sum += current_tan;
      }
      return result.scale(1 / tan_sum);
    }

    constexpr Point<coordinate_type> circumcenter() const {
      return ((*this)[0] + (*this)[1] + (*this)[2] - orthocenter()).scale(0.5);
    }

    constexpr Point<coordinate_type> incenter() const {
      Point<coordinate_type> result{0, 0};
      for (size_t i = 0; i < 3; ++i) {
        result += (*this)[i].scale((*this)(i + 1).length());
      }
      return result.scale(1 / this->perimeter());
    }

    Circle<coordinate_type> circumscribedCircle() const {
      const auto RADIUS =
          (*this)(0).length() * (*this)(1).length() * (*this)(2).length() / (4 * this->area());
      return Circle<coordinate_type>(circumcenter(), RADIUS);
    }

    Circle<coordinate_type> inscribedCircle() const {
      const auto RADIUS = this->area() / (this->perimeter() / 2);
      return Circle<coordinate_type>(incenter(), RADIUS);
    }

    Circle<coordinate_type> ninePointsCircle() const {
      const auto CENTER = (circumcenter() + orthocenter()).scale(0.5);
      return Circle<coordinate_type>(CENTER,
                                     (CENTER >> ((*this)[0] + (*this)[1]).scale(0.5)).length());
    }

    constexpr Line EulerLine() const {
      return Line(circumcenter(), orthocenter());
    }
};

}  // namespace geometry

using Point = geometry::Point<long double>;
using Line = geometry::Line;
using Shape = geometry::Shape<long double>;
using Polygon = geometry::Polygon<long double>;
using Ellipse = geometry::Ellipse<long double>;
using Rectangle = geometry::Rectangle<long double>;
using Square = geometry::Square<long double>;
using Triangle = geometry::Triangle<long double>;
using Circle = geometry::Circle<long double>;

#endif  // GEOMETRY_H
