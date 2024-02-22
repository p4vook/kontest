#ifndef MATRIX_H
#define MATRIX_H

#include <algorithm>
#include <array>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <istream>
#include <iterator>
#include <numeric>
#include <ostream>
#include <type_traits>

namespace Math {
class BigInteger {
  private:
    using block_type = uint32_t;

    bool isNegative;
    static const int64_t MOD = 1'000'000'000;
    std::basic_string<block_type> blocks;

    static const size_t FAST_MULTIPLY_THRESHOLD = 15;

  public:
    static const unsigned MOD_BASE = 10;
    static const unsigned MOD_LENGTH = 9;

    BigInteger() : isNegative{false}, blocks{block_type()} {
    }

    BigInteger(int n) : BigInteger(static_cast<uint32_t>(std::abs(n))) {
      isNegative = std::signbit(n);
    }

    BigInteger(long long n) : BigInteger(static_cast<uint64_t>(std::abs(n))) {
      isNegative = std::signbit(n);
    }

    BigInteger(std::unsigned_integral auto n) : isNegative(false) {
      if (!n) {
        blocks = {block_type()};
        return;
      }
      auto current = n;
      while (current) {
        blocks.push_back(static_cast<block_type>(current % MOD));
        current /= MOD;
      }
    }

    BigInteger& operator=(const BigInteger& other) = default;
    BigInteger& operator=(BigInteger&& other) = default;
    BigInteger(const BigInteger& other) = default;
    BigInteger(BigInteger&& other) = default;
    ~BigInteger() = default;

    constexpr void negate() noexcept {
      isNegative = !isNegative;
    }

    constexpr bool negative() const noexcept {
      return isNegative;
    }

    constexpr void absolute() noexcept {
      if (negative()) {
        negate();
      }
    }

    constexpr explicit operator bool() const noexcept {
      return (blocks.size() > 1 || blocks[0] != 0);
    }

    inline BigInteger operator-() const& {
      BigInteger result = *this;
      return -std::move(result);
    }

    inline BigInteger operator-() && {
      negate();
      return *this;
    }

    inline BigInteger operator<<(size_t shift) && {
      if (*this) {
        blocks.insert(blocks.begin(), shift, block_type());
      }
      return *this;
    }

    inline BigInteger operator<<(size_t shift) const& {
      BigInteger result = *this;
      return std::move(result) << shift;
    }

    inline BigInteger operator>>(size_t shift) && {
      if (shift >= blocks.size()) {
        blocks = {};
        return *this;
      }
      blocks.erase(blocks.begin(), blocks.begin() + shift);
      return *this;
    }

    inline BigInteger operator>>(size_t shift) const& {
      BigInteger result = *this;
      return std::move(result) >> shift;
    }

  private:
    // adds a BigInteger, assuming result will be of same sign
    template <bool same_sign>
    [[gnu::always_inline]] constexpr void add(const BigInteger& other) {
      if (blocks.size() < other.blocks.size()) {
        blocks.resize(other.blocks.size());
      }
      int64_t carry = 0;
      for (size_t i = 0; i < other.blocks.size() || (carry && i < blocks.size()); ++i) {
        carry += blocks[i];
        if (i < other.blocks.size()) {
          carry += (same_sign ? static_cast<int64_t>(other.blocks[i])
                              : -static_cast<int64_t>(other.blocks[i]));
        }
        if (carry < 0) {
          blocks[i] = static_cast<block_type>(carry + MOD);
          carry = -1;
        } else if (carry < MOD) {
          blocks[i] = static_cast<block_type>(carry);
          carry = 0;
        } else {
          blocks[i] = static_cast<block_type>(carry - MOD);
          carry = 1;
        }
      }
      if (carry) {
        assert(carry > 0 && "not same sign");
        blocks.push_back(static_cast<block_type>(carry));
      }
      while (blocks.back() == 0 && blocks.size() > 1) {
        blocks.pop_back();
      }
    }

    [[gnu::always_inline]] constexpr void multiplyBlock(block_type multiplier) {
      if (!multiplier) {
        blocks = {block_type()};
        return;
      }
      int64_t carry = 0;
      for (unsigned int& block : blocks) {
        carry += static_cast<int64_t>(block) * multiplier;
        block = static_cast<block_type>(carry % MOD);
        carry /= MOD;
      }
      if (carry) {
        blocks.push_back(static_cast<block_type>(carry));
      }
    }

    BigInteger blockMultiplySlow(const BigInteger& other) const&;

    static BigInteger blockMultiply(const BigInteger& lhs, const BigInteger& rhs);

    [[gnu::always_inline]] inline block_type divideSmall(const BigInteger& divisor) const& {
      block_type left = 0;
      block_type right = MOD;
      while (right > left + 1) {
        block_type middle = std::midpoint(left, right);
        BigInteger candidate = divisor;
        candidate.multiplyBlock(middle);
        if (candidate <= *this) {
          left = middle;
        } else {
          right = middle;
        }
      }
      return left;
    }

    // leave remainder in current BigInteger
    [[gnu::always_inline]] inline BigInteger blockDivideQuotient(const BigInteger& divisor) & {
      BigInteger result = 0;
      if (divisor > *this) {
        return result;
      }
      size_t current_pos = blocks.size();
      BigInteger current = 0;
      while (current_pos > 0) {
        while (current_pos > 0 && current < divisor) {
          current = std::move(current) << 1;
          result = std::move(result) << 1;
          current += BigInteger(blocks[--current_pos]);
        }
        if (current < divisor) {
          break;
        }
        block_type last_digit = current.divideSmall(divisor);
        result += BigInteger(last_digit);

        BigInteger temp_divisor = divisor;
        temp_divisor.multiplyBlock(last_digit);
        current -= temp_divisor;
      }
      blocks = current.blocks;
      return result;
    }

  public:
    constexpr std::strong_ordering absoluteCompare(const BigInteger& other) const& noexcept {
      if (blocks.size() != other.blocks.size()) {
        return blocks.size() <=> other.blocks.size();
      }
      for (auto it1 = blocks.rbegin(), it2 = other.blocks.rbegin(); it1 != blocks.rend();
           ++it1, ++it2) {
        auto cmp_res = *it1 <=> *it2;
        if (cmp_res != std::strong_ordering::equal) {
          return cmp_res;
        }
      }
      return std::strong_ordering::equal;
    }

    constexpr bool operator==(const BigInteger& other) const& noexcept {
      return (negative() == other.negative() ||
              (!static_cast<bool>(*this) && !static_cast<bool>(other))) &&
             absoluteCompare(other) == std::strong_ordering::equal;
    }

    constexpr std::strong_ordering operator<=>(const BigInteger& other) const& noexcept {
      if (negative() == other.negative()) {
        return (negative() ? other.absoluteCompare(*this) : absoluteCompare(other));
      }
      if (*this == other) {
        return std::strong_ordering::equal;
      } else if (negative()) {
        return std::strong_ordering::less;
      } else {
        return std::strong_ordering::greater;
      }
    }

    inline std::string toString() const& {
      if (!*this) {
        return "0";
      }
      std::string result;
      for (size_t i = 0; i < blocks.size(); ++i) {
        auto block = blocks[i];
        for (unsigned digit = 0; (i + 1 < blocks.size() ? digit < MOD_LENGTH : block > 0);
             ++digit) {
          result.push_back(static_cast<char>('0' + block % MOD_BASE));
          block /= MOD_BASE;
        }
      }
      if (negative()) {
        result.push_back('-');
      }
      reverse(result.begin(), result.end());
      return result;
      return "";
    }

    BigInteger& operator*=(const BigInteger& rhs);
    BigInteger& operator+=(const BigInteger& rhs);
    BigInteger& operator-=(const BigInteger& rhs);
    BigInteger& operator/=(const BigInteger& rhs);
    BigInteger& operator%=(const BigInteger& rhs);

    [[gnu::always_inline]] inline BigInteger& operator++() {
      return *this += 1;
    }

    [[gnu::always_inline]] inline BigInteger& operator--() {
      return *this += -1;
    }

    inline BigInteger operator++(int) {
      BigInteger result = *this;
      ++(*this);
      return result;
    }

    inline BigInteger operator--(int) {
      BigInteger result = *this;
      --(*this);
      return result;
    }

    inline friend std::ostream& operator<<(std::ostream& out, const BigInteger& value);
    inline friend std::istream& operator>>(std::istream& in, BigInteger& value);
};

inline BigInteger operator+(const BigInteger& lhs, const BigInteger& rhs);

[[gnu::always_inline]] inline BigInteger& BigInteger::operator+=(const BigInteger& rhs) {
  if (absoluteCompare(rhs) < 0) {
    *this = rhs + *this;
  } else {
    if (negative() == rhs.negative()) {
      add<true>(rhs);
    } else {
      add<false>(rhs);
    }
  }
  return *this;
}

inline BigInteger operator+(BigInteger&& lhs, BigInteger&& rhs) {
  return lhs += rhs;
}

inline BigInteger operator+(BigInteger&& lhs, const BigInteger& rhs) {
  return lhs += rhs;
}

inline BigInteger operator+(const BigInteger& lhs, BigInteger&& rhs) {
  return rhs += lhs;
}

inline BigInteger operator+(const BigInteger& lhs, const BigInteger& rhs) {
  BigInteger result = lhs;
  return std::move(result) + rhs;
}

inline BigInteger& BigInteger::operator-=(const BigInteger& rhs) {
  return *this += -rhs;
}

inline BigInteger operator-(BigInteger&& lhs, const BigInteger& rhs) {
  return lhs -= rhs;
}

inline BigInteger operator-(const BigInteger& lhs, const BigInteger& rhs) {
  BigInteger result(lhs);
  return std::move(result) - rhs;
}

[[gnu::always_inline]] inline BigInteger BigInteger::blockMultiplySlow(
    const BigInteger& other) const& {
  BigInteger result = 0;
  for (size_t i = 0; i < other.blocks.size(); ++i) {
    BigInteger temp = *this;
    temp.absolute();
    temp.multiplyBlock(other.blocks[i]);
    result += std::move(temp) << i;
  }
  return result;
}

inline BigInteger BigInteger::blockMultiply(const BigInteger& lhs, const BigInteger& rhs) {
  if (rhs.blocks.size() <= BigInteger::FAST_MULTIPLY_THRESHOLD) {
    return lhs.blockMultiplySlow(rhs);
  }
  if (lhs.blocks.size() <= BigInteger::FAST_MULTIPLY_THRESHOLD) {
    return rhs.blockMultiplySlow(lhs);
  }
  size_t middle = std::max(lhs.blocks.size(), rhs.blocks.size()) / 2;
  BigInteger lhs_high = lhs >> middle;
  lhs_high.absolute();
  BigInteger rhs_high = rhs >> middle;
  rhs_high.absolute();
  BigInteger lhs_low = lhs;
  lhs_low.absolute();
  lhs_low.blocks.resize(middle);
  BigInteger rhs_low = rhs;
  rhs_low.absolute();
  rhs_low.blocks.resize(middle);
  BigInteger LL = blockMultiply(lhs_low, rhs_low);
  BigInteger HH = blockMultiply(lhs_high, rhs_high);
  BigInteger T = blockMultiply(lhs_low += lhs_high, rhs_low += rhs_high);

  return (HH << (2 * middle)) + ((T - (LL + HH)) << middle) + LL;
}

inline BigInteger& BigInteger::operator*=(const BigInteger& rhs) {
  BigInteger&& result = BigInteger::blockMultiply(*this, rhs);
  if (negative() ^ rhs.negative()) {
    result.negate();
  }
  return *this = std::move(result);
}

inline BigInteger operator*(const BigInteger& lhs, const BigInteger& rhs) {
  BigInteger copy(lhs);
  copy *= rhs;
  return copy;
}

inline BigInteger& BigInteger::operator/=(const BigInteger& rhs) {
  bool is_negative = negative();
  absolute();
  BigInteger divisor = rhs;
  is_negative ^= divisor.negative();
  divisor.absolute();
  *this = blockDivideQuotient(divisor);
  if (is_negative) {
    negate();
  }
  return *this;
}

inline BigInteger& BigInteger::operator%=(const BigInteger& rhs) {
  bool is_negative = negative();
  absolute();
  BigInteger divisor = rhs;
  divisor.absolute();
  blockDivideQuotient(divisor);
  if (is_negative) {
    negate();
  }
  return *this;
}

inline BigInteger operator/(const BigInteger& lhs, const BigInteger& rhs) {
  BigInteger result = lhs;
  return result /= rhs;
}

inline BigInteger operator%(const BigInteger& lhs, const BigInteger& rhs) {
  BigInteger result = lhs;
  return result %= rhs;
}

inline BigInteger operator""_bi(unsigned long long n) {
  return n;
}

inline std::ostream& operator<<(std::ostream& out, const BigInteger& number) {
  typename std::ostream::sentry sentry(out);
  if (sentry) {
    out << number.toString();
  }
  return out;
}

inline std::istream& operator>>(std::istream& in, BigInteger& number) {
  typename std::istream::sentry sentry(in);
  if (sentry) {
    std::string str;
    for (int next = in.peek(); next == '-' || ('0' <= next && next <= '9'); next = in.peek()) {
      str.push_back(static_cast<char>(in.get()));
    }
    number = BigInteger();
    size_t start = 0;
    if (str[start] == '-') {
      ++start;
    }
    size_t real_size = str.size() - start;
    size_t pos = str.size();
    number.blocks.resize((real_size + BigInteger::MOD_LENGTH - 1) / BigInteger::MOD_LENGTH);
    for (size_t i = 0; i + 1 < number.blocks.size(); ++i) {
      pos -= BigInteger::MOD_LENGTH;
      number.blocks[i] = std::stoi(str.substr(pos, BigInteger::MOD_LENGTH));
    }
    number.blocks.back() = std::stoi(str.substr(start, pos - start));

    if (str[0] == '-') {
      number.negate();
    }
  }
  return in;
}

[[gnu::always_inline]] inline BigInteger gcd(BigInteger a, BigInteger b) {
  a.absolute();
  b.absolute();
  while (b) {
    BigInteger temp = b;
    b = a % b;
    a = std::move(temp);
  }
  return a;
}

class Rational {
    BigInteger numerator = 0;
    BigInteger denominator = 1;

  public:
    Rational() = default;

    Rational(int value) : numerator(value), denominator(1) {
    }

    Rational(BigInteger value) : numerator(std::move(value)), denominator(1) {
    }

    Rational(BigInteger numerator, BigInteger denominator)
        : numerator(std::move(numerator)), denominator(std::move(denominator)) {
      normalize();
    }

    explicit inline operator bool() const {
      return static_cast<bool>(numerator);
    }

    inline void normalizeDenominator() {
      if (denominator.negative()) {
        numerator.negate();
        denominator.negate();
      }
    }

    inline void normalize() {
      normalizeDenominator();
      BigInteger divisor = gcd(numerator, denominator);
      numerator /= divisor;
      denominator /= divisor;
    }

    inline void invert() {
      std::swap(numerator, denominator);
      normalizeDenominator();
    }

    inline Rational& operator+=(const Rational& other) & {
      numerator *= other.denominator;
      numerator += other.numerator * denominator;
      denominator *= other.denominator;
      return *this;
    }

    inline Rational operator-() const {
      Rational result = *this;
      result.numerator.negate();
      return result;
    }

    inline Rational& operator-=(const Rational& other) & {
      return (*this) += (-other);
    }

    inline Rational& operator*=(const Rational& other) & {
      numerator *= other.numerator;
      denominator *= other.denominator;
      normalize();
      return *this;
    }

    inline Rational& operator/=(const Rational& other) & {
      numerator *= other.denominator;
      denominator *= other.numerator;
      normalize();
      return *this;
    }

    inline bool operator==(const Rational& other) const& {
      return numerator * other.denominator == other.numerator * denominator;
    }

    inline std::strong_ordering operator<=>(const Rational& other) const& {
      return numerator * other.denominator <=> other.numerator * denominator;
    }

    inline std::string toString() {
      normalize();
      std::string result = numerator.toString();
      if (denominator != 1) {
        result.push_back('/');
        result += denominator.toString();
      }
      return result;
    }

    inline std::string toString() const {
      Rational temp = *this;
      return temp.toString();
    }

    inline std::string asDecimal(size_t precision = 0) {
      normalizeDenominator();
      BigInteger temp = numerator;
      temp.absolute();
      temp = temp << (precision / BigInteger::MOD_LENGTH);
      for (unsigned i = 0; i < precision % BigInteger::MOD_LENGTH; ++i) {
        temp *= BigInteger::MOD_BASE;
      }
      temp += denominator / 2;
      temp /= denominator;
      std::string result = temp.toString();
      if (result.size() <= precision) {
        result.insert(result.begin(), precision + 1 - result.size(), '0');
      }
      result.insert(result.end() - precision, '.');
      if (numerator.negative()) {
        result.insert(result.begin(), '-');
      }
      return result;
    }

    inline std::string asDecimal(size_t precision) const {
      Rational temp = *this;
      return temp.asDecimal(precision);
    }

    inline explicit operator double() const {
      const size_t TO_DOUBLE_PRECISION = 16;
      return std::stod(asDecimal(TO_DOUBLE_PRECISION));
    }
};

inline Rational operator+(Rational&& lhs, Rational&& rhs) {
  return lhs += rhs;
}

inline Rational operator+(Rational&& lhs, const Rational& rhs) {
  return lhs += rhs;
}

inline Rational operator+(const Rational&& lhs, Rational&& rhs) {
  return rhs += lhs;
}

inline Rational operator-(Rational&& lhs, const Rational& rhs) {
  return lhs -= rhs;
}

inline Rational operator*(Rational&& lhs, Rational&& rhs) {
  return lhs *= rhs;
}

inline Rational operator*(Rational&& lhs, const Rational& rhs) {
  return lhs *= rhs;
}

inline Rational operator*(const Rational& lhs, Rational&& rhs) {
  return rhs *= lhs;
}

inline Rational operator/(Rational&& lhs, const Rational& rhs) {
  return lhs /= rhs;
}

inline Rational operator+(const Rational& lhs, const Rational& rhs) {
  Rational result(lhs);
  return result += rhs;
}

inline Rational operator-(const Rational& lhs, const Rational& rhs) {
  Rational result(lhs);
  return result -= rhs;
}

inline Rational operator*(const Rational& lhs, const Rational& rhs) {
  Rational result(lhs);
  return result *= rhs;
}

inline Rational operator/(const Rational& lhs, const Rational& rhs) {
  Rational result(lhs);
  return result /= rhs;
}

inline std::ostream& operator<<(std::ostream& out, const Rational& rational) {
  return out << rational.toString();
}

inline std::istream& operator>>(std::istream& in, Rational& rational) {
  std::istream::sentry sentry(in);
  if (sentry) {
    BigInteger numerator;
    in >> numerator;
    BigInteger denominator = 1;
    if (in.peek() == '/') {
      in.get();
      in >> denominator;
    }
    rational = Rational(numerator, denominator);
  }
  return in;
}

template <std::integral T>
constexpr bool isPrime(T n) {
  if (n == 1) {
    return false;
  }
  for (T cand = 2; cand * cand <= n; ++cand) {
    if (n % cand == 0) {
      return false;
    }
  }
  return true;
}

template <size_t N>
class Residue {
  private:
    size_t value;

    constexpr Residue(size_t value_) : value(value_) {
    }

    static constexpr size_t fastMod(size_t sum_of_two) {
      return (sum_of_two < N ? sum_of_two : sum_of_two - N);
    }

    static constexpr size_t binPow(size_t base, size_t power) {
      size_t res = 1;
      while (power) {
        if (power & 1) {
          res = res * base % N;
        }
        base = base * base % N;
        power >>= 1;
      }
      return res;
    }

  public:
    constexpr Residue() : value(0) {
    }

    constexpr Residue(int value) : value(value >= 0 ? value % N : fastMod(N - abs(value) % N)) {
    }

    explicit constexpr operator int() const {
      return static_cast<int>(value);
    }

    explicit constexpr operator bool() const {
      return static_cast<bool>(value);
    }

    constexpr Residue operator-() const {
      return Residue(value == 0 ? value : N - value);
    }

    constexpr Residue operator+(const Residue& other) const {
      return Residue(fastMod(value + other.value));
    }

    constexpr Residue operator-(const Residue& other) const {
      return (*this) + (-other);
    }

    constexpr Residue operator*(const Residue& other) const {
      return Residue(value * other.value % N);
    }

    constexpr Residue inverse() const
      requires(isPrime(N))
    {
      return Residue(binPow(value, N - 2));
    }

    constexpr Residue operator/(const Residue& other) const
      requires(isPrime(N))
    {
      return (*this) * other.inverse();
    }

    constexpr bool operator==(const Residue& other) const = default;

    constexpr size_t getValue() const {
      return value;
    }
};

template <size_t N>
inline std::ostream& operator<<(std::ostream& out, const Residue<N>& residue) {
  return out << "[" << residue.getValue() << "]";
}

namespace Concepts {
template <typename From, typename To>
concept castable_to = requires {
  { static_cast<To>(std::declval<From>()) };
};

template <typename T>
struct WithZero {};

template <std::default_initializable T>
struct WithZero<T> {
    static inline T zero() {
      return {};
    }
};

template <typename T>
concept HasZero = requires() {
  { WithZero<T>::zero() } -> castable_to<T>;
};

template <typename T>
concept TriviallyReversible = HasZero<T> && requires(const T& a) {
  { WithZero<T>::zero() - a } -> castable_to<T>;
};

template <typename T>
struct Reversible {};

template <TriviallyReversible T>
struct Reversible<T> {
    static inline T reverse(const T& x) {
      return WithZero<T>::zero() - x;
    }
};

template <typename T>
concept IsReversible = requires(const T& a) {
  { Reversible<T>::reverse(a) } -> castable_to<T>;
};

template <typename T>
struct ZeroCompare {};

template <castable_to<bool> T>
struct ZeroCompare<T> {
    static inline bool isZero(const T& x) {
      return !static_cast<bool>(x);
    }
};

template <typename T>
concept ZeroComparable = requires(const T& x) {
  { ZeroCompare<T>::isZero(x) } -> std::same_as<bool>;
};

template <typename T>
struct WithOne {};

template <typename T>
  requires std::is_constructible_v<T, int>
struct WithOne<T> {
    static inline T one()
      requires std::is_constructible_v<T, int>
    {
      return T(1);
    }
};

template <typename T>
concept HasOne = requires(const T& x) {
  { WithOne<T>::one() } -> castable_to<T>;
};

template <typename T>
concept TriviallyInvertible = HasOne<T> && requires(const T& a) {
  { WithOne<T>::one() / a } -> castable_to<T>;
};

template <typename T>
struct Invertible {};

template <TriviallyInvertible T>
struct Invertible<T> {
    static inline T invert(const T& a) {
      return WithOne<T>::one() / a;
    }
};

template <typename T>
concept IsInvertible = requires(const T& a) {
  { Invertible<T>::invert(a) } -> castable_to<T>;
};

template <typename T>
concept Ring =
    std::copyable<T> && HasZero<T> && IsReversible<T> && requires(const T& a, const T& b) {
      // clang-format off
      { a * b } -> castable_to<T>;
      // clang-format on
      { a + b } -> castable_to<T>;
    };

template <typename T>
concept Field = Ring<T> && HasOne<T> && IsInvertible<T>;

template <typename T>
concept HasPlusAssign = requires(T& a, const T& b) {
  { a += b } -> std::same_as<T&>;
};

template <typename T>
concept HasMultiplyAssign = requires(T& a, const T& b) {
  { a *= b } -> std::same_as<T&>;
};

template <typename T>
struct PlusAssign {
    static T& plusAssign(T& lhs, const T& rhs) {
      return lhs = std::move(lhs) + rhs;
    }
};

template <HasPlusAssign T>
struct PlusAssign<T> {
    static T& plusAssign(T& lhs, const T& rhs) {
      return lhs += rhs;
    }
};

template <typename T>
struct MultiplyAssign {
    static T& multiplyAssign(T& lhs, const T& rhs) {
      return lhs = std::move(lhs) * rhs;
    }
};

template <HasMultiplyAssign T>
struct MultiplyAssign<T> {
    static T& multiplyAssign(T& lhs, const T& rhs) {
      return lhs *= rhs;
    }
};

}  // namespace Concepts

template <size_t N, size_t M, Concepts::Ring R, class E>
class MatrixExpression {
  public:
    constexpr R operator[](size_t row, size_t column) const {
      return static_cast<const E&>(*this)[row, column];
    }

    constexpr auto transposed() const;
};

template <class E>
using expression_ref = std::conditional_t<E::IS_LEAF, const E&, const E>;

template <size_t N, size_t M, Concepts::Ring R, class E>
class MatrixTranspose : public MatrixExpression<M, N, R, MatrixTranspose<N, M, R, E>> {
  private:
    expression_ref<E> expression;

  public:
    static constexpr bool IS_LEAF = false;

    constexpr MatrixTranspose(const E& expression_) : expression(expression_) {
    }

    constexpr R operator[](size_t row, size_t column) const {
      return expression[column, row];
    }
};

template <size_t N, size_t M, Concepts::Ring R, class E>
constexpr auto MatrixExpression<N, M, R, E>::transposed() const {
  return MatrixTranspose<N, M, R, E>(static_cast<const E&>(*this));
}

template <size_t N, size_t M, Concepts::Ring R, class E1, class E2>
constexpr bool operator==(const MatrixExpression<N, M, R, E1>& lhs,
                          const MatrixExpression<N, M, R, E2>& rhs) {
  for (size_t row = 0; row < N; ++row) {
    for (size_t column = 0; column < M; ++column) {
      if (lhs[row, column] != rhs[row, column]) {
        return false;
      }
    }
  }
  return true;
}

template <size_t N, size_t M, Concepts::Ring R>
class ZeroMatrix : public MatrixExpression<N, M, R, ZeroMatrix<N, M, R>> {
  public:
    static constexpr bool IS_LEAF = true;

    constexpr R operator[](size_t, size_t) const {
      return Concepts::WithZero<R>::zero();
    }
};

template <size_t N, size_t M, Concepts::Ring R>
  requires Concepts::HasOne<R>
class UnaryMatrix : public MatrixExpression<N, M, R, UnaryMatrix<N, M, R>> {
  public:
    static constexpr bool IS_LEAF = true;

    constexpr R operator[](size_t row, size_t column) const {
      return (row == column ? Concepts::WithOne<R>::one() : Concepts::WithZero<R>::zero());
    }
};

template <size_t N, size_t M, Concepts::Ring R, class E>
class MatrixNegate : public MatrixExpression<N, M, R, MatrixNegate<N, M, R, E>> {
  private:
    expression_ref<E> expression;

  public:
    static constexpr bool IS_LEAF = false;

    constexpr MatrixNegate(const E& expression_) : expression(expression_) {
    }

    constexpr R operator[](size_t row, size_t column) const {
      return Concepts::Reversible<R>::reverse(expression[row, column]);
    }
};

template <size_t N, size_t M, Concepts::Ring R = Rational>
class Matrix : public MatrixExpression<N, M, R, Matrix<N, M, R>> {
  private:
    std::array<std::array<R, M>, N> array;

  public:
    static constexpr bool IS_LEAF = true;

    constexpr const R& operator[](size_t row, size_t column) const {
      return array[row][column];
    }

    constexpr R& operator[](size_t row, size_t column) {
      return array[row][column];
    }

  private:
    template <class E, class BinaryOperation>
    constexpr void applyBinaryOperation(const MatrixExpression<N, M, R, E>& rhs,
                                        BinaryOperation binary_op) {
      for (size_t row = 0; row < N; ++row) {
        for (size_t col = 0; col < M; ++col) {
          binary_op((*this)[row, col], rhs[row, col]);
        }
      }
    }

    template <class UnaryOperation>
    constexpr void applyUnaryOperation(UnaryOperation unary_op) {
      for (size_t row = 0; row < N; ++row) {
        for (size_t col = 0; col < M; ++col) {
          unary_op((*this)[row, col]);
        }
      }
    }

  public:
    template <class E>
    constexpr Matrix(const MatrixExpression<N, M, R, E>& expression) {
      // this is idiotic, as it requires R to be default-constructible
      // unfortunately, I see no other sane way to implement this
      // perhaps, some variadic magic could work
      for (size_t row = 0; row < N; ++row) {
        for (size_t column = 0; column < M; ++column) {
          (*this)[row, column] = expression[row, column];
        }
      }
    }

    constexpr Matrix() : Matrix(ZeroMatrix<N, M, R>()) {
    }

    constexpr Matrix(const std::initializer_list<std::initializer_list<R>>& values) {
      // TODO check for size?
      // static_assert(values.size() == N);
      size_t row_index = 0;
      for (const auto& row : values) {
        // TODO check for size?
        // static_assert(values.size() == M);
        size_t column_index = 0;
        for (const auto& element : row) {
          (*this)[row_index, column_index] = element;
          ++column_index;
        }
        ++row_index;
      }
    }

    constexpr auto operator-() const {
      return MatrixNegate<N, M, R, Matrix<N, M, R>>(*this);
    }

    template <class E>
    constexpr Matrix& operator+=(const MatrixExpression<N, M, R, E>& other) {
      applyBinaryOperation(other, Concepts::PlusAssign<R>::plusAssign);
      return *this;
    }

    template <class E>
    constexpr Matrix& operator-=(const MatrixExpression<N, M, R, E>& other) {
      applyBinaryOperation(other, [](R& lhs, const R& rhs) {
        Concepts::PlusAssign<R>::plusAssign(lhs, Concepts::Reversible<R>::reverse(rhs));
      });
      return *this;
    }

    constexpr Matrix& operator*=(const R& multiplier) {
      applyUnaryOperation(
          [&](R& value) { Concepts::MultiplyAssign<R>::multiplyAssign(value, multiplier); });
      return *this;
    }

    constexpr R trace() const
      requires(N == M)
    {
      R result = Concepts::WithZero<R>::zero();
      for (size_t i = 0; i < N; ++i) {
        Concepts::PlusAssign<R>::plusAssign(result, (*this)[i, i]);
      }
      return result;
    }

    constexpr std::array<R, M> getRow(size_t row) const {
      return array[row];
    }

    constexpr std::array<R, N> getColumn(size_t column) const {
      std::array<R, N> result;
      for (size_t row = 0; row < N; ++row) {
        result[row] = (*this)[row, column];
      }
      return result;
    }

    constexpr void subtractRow(size_t lhs_row, size_t rhs_row, size_t column_start, R multiplier) {
      for (size_t column = column_start; column < M; ++column) {
        Concepts::PlusAssign<R>::plusAssign((*this)[lhs_row, column],
                                          Concepts::Reversible<R>::reverse((*this)[rhs_row, column] * multiplier));
      }
    }

    constexpr std::pair<size_t, R> gaussianEliminate()
      requires Concepts::Field<R> && Concepts::ZeroComparable<R>
    {
      R delta = Concepts::WithOne<R>::one();
      R minus_one = Concepts::Reversible<R>::reverse(Concepts::WithOne<R>::one());
      size_t row = 0;
      size_t column = 0;
      for (; row < N; ++row) {
        for (; column < M; ++column) {
          size_t next_row = row;
          while (next_row < N && Concepts::ZeroCompare<R>::isZero((*this)[next_row, column])) {
            ++next_row;
          }
          if (next_row >= N) {
            continue;
          }

          if (next_row != row) {
            std::swap(array[row], array[next_row]);
            Concepts::MultiplyAssign<R>::multiplyAssign(delta, minus_one);
          }
          break;
        }
        if (column >= M) {
          break;
        }

        R main_inverse = Concepts::Invertible<R>::invert((*this)[row, column]);
        for (size_t other_row = row + 1; other_row < N; ++other_row) {
          subtractRow(other_row, row, column, main_inverse * (*this)[other_row, column]);
        }
        ++column;
      }
      return {row, delta};
    }

    constexpr void reverseGaussian()
      requires Concepts::Field<R> && Concepts::ZeroComparable<R>
    {
      if (N == 0 || M == 0) {
        return;
      }
      for (size_t row = N - 1;; --row) {
        size_t first_nonzero = std::find_if(array[row].begin(), array[row].end(),
                                            [](const auto& element) {
                                              return !Concepts::ZeroCompare<R>::isZero(element);
                                            }) -
                               array[row].begin();
        if (first_nonzero == M) {
          continue;
        }
        R main_inverse = Concepts::Invertible<R>::invert((*this)[row, first_nonzero]);
        for (size_t column = first_nonzero; column < M; ++column) {
          Concepts::MultiplyAssign<R>::multiplyAssign((*this)[row, column], main_inverse);
        }
        for (size_t other_row = 0; other_row < row; ++other_row) {
          subtractRow(other_row, row, first_nonzero, (*this)[other_row, first_nonzero]);
        }
        if (row == 0) {
          break;
        }
      }
    }

    constexpr size_t rank() const
      requires Concepts::Field<R> && Concepts::ZeroComparable<R>
    {
      Matrix copy(*this);
      return copy.gaussianEliminate().first;
    }

    constexpr R det() const
      requires Concepts::Field<R> && Concepts::ZeroComparable<R> && (N == M)
    {
      Matrix copy(*this);
      auto [rank, delta] = copy.gaussianEliminate();
      if (rank < N) {
        return Concepts::WithZero<R>::zero();
      }
      R res = Concepts::WithOne<R>::one();
      for (size_t i = 0; i < N; ++i) {
        Concepts::MultiplyAssign<R>::multiplyAssign(res, copy[i, i]);
      }
      return res * delta;
    }

    constexpr Matrix inverted() const
      requires Concepts::Field<R> && Concepts::ZeroComparable<R> && (N == M)
    {
      Matrix<N, N + N, R> copy = concatenate(*this, UnaryMatrix<N, N, R>());
      auto [rank, delta] = copy.gaussianEliminate();
      if (rank < N) {
        throw std::invalid_argument("matrix determinant is zero");
      }
      copy.reverseGaussian();
      Matrix result;
      for (size_t row = 0; row < N; ++row) {
        for (size_t column = 0; column < N; ++column) {
          result[row, column] = copy[row, N + column];
        }
      }
      return result;
    }

    constexpr void invert()
      requires Concepts::Field<R> && Concepts::ZeroComparable<R> && (N == M)
    {
      *this = inverted();
    }
};

template <size_t N, size_t M, Concepts::Ring R, class E1, class E2>
class MatrixSum : public MatrixExpression<N, M, R, MatrixSum<N, M, R, E1, E2>> {
  private:
    expression_ref<E1> lhs;
    expression_ref<E2> rhs;

  public:
    static constexpr bool IS_LEAF = false;

    constexpr MatrixSum(const E1& lhs_, const E2& rhs_) : lhs(lhs_), rhs(rhs_) {
    }

    constexpr R operator[](size_t row, size_t column) const {
      return lhs[row, column] + rhs[row, column];
    }
};

template <size_t N, size_t M, Concepts::Ring R, class E1, class E2>
constexpr MatrixSum<N, M, R, E1, E2> operator+(const MatrixExpression<N, M, R, E1>& lhs,
                                               const MatrixExpression<N, M, R, E2>& rhs) {
  return MatrixSum<N, M, R, E1, E2>(static_cast<const E1&>(lhs), static_cast<const E2&>(rhs));
}

template <size_t N, size_t M, Concepts::Ring R, class E1, class E2>
class MatrixDifference : public MatrixExpression<N, M, R, MatrixDifference<N, M, R, E1, E2>> {
  private:
    expression_ref<E1> lhs;
    expression_ref<E2> rhs;

  public:
    static constexpr bool IS_LEAF = false;

    constexpr MatrixDifference(const E1& lhs_, const E2& rhs_) : lhs(lhs_), rhs(rhs_) {
    }

    constexpr R operator[](size_t row, size_t column) const {
      return lhs[row, column] + Concepts::Reversible<R>::reverse(rhs[row, column]);
    }
};

template <size_t N, size_t M, Concepts::Ring R, class E1, class E2>
constexpr MatrixDifference<N, M, R, E1, E2> operator-(const MatrixExpression<N, M, R, E1>& lhs,
                                                      const MatrixExpression<N, M, R, E2>& rhs) {
  return MatrixDifference<N, M, R, E1, E2>(static_cast<const E1&>(lhs),
                                           static_cast<const E2&>(rhs));
}

template <size_t N, size_t M, size_t K, Concepts::Ring R, class E1, class E2>
class MatrixProduct : public MatrixExpression<N, M, R, MatrixProduct<N, M, K, R, E1, E2>> {
  private:
    expression_ref<E1> lhs;
    expression_ref<E2> rhs;

  public:
    static constexpr bool IS_LEAF = false;

    constexpr MatrixProduct(const E1& lhs_, const E2& rhs_) : lhs(lhs_), rhs(rhs_) {
    }

    constexpr R operator[](size_t row, size_t column) const {
      R result = Concepts::WithZero<R>::zero();
      for (size_t k = 0; k < K; ++k) {
        Concepts::PlusAssign<R>::plusAssign(result, lhs[row, k] * rhs[k, column]);
      }
      return result;
    }
};

template <size_t N, size_t M, size_t K, Concepts::Ring R, class E1, class E2>
constexpr MatrixProduct<N, M, K, R, E1, E2> operator*(const MatrixExpression<N, K, R, E1>& lhs,
                                                      const MatrixExpression<K, M, R, E2>& rhs) {
  return MatrixProduct<N, M, K, R, E1, E2>(static_cast<const E1&>(lhs),
                                           static_cast<const E2&>(rhs));
}

template <size_t N, size_t M, size_t K, Concepts::Ring R, class E>
constexpr Matrix<N, M, R>& operator*=(Matrix<N, K, R>& lhs,
                                      const MatrixExpression<K, M, R, E>& rhs) {
  return lhs = (lhs * rhs);
}

template <size_t N, size_t M1, size_t M2, Concepts::Ring R, class E1, class E2>
class MatrixConcatenate
    : public MatrixExpression<N, M1 + M2, R, MatrixConcatenate<N, M1, M2, R, E1, E2>> {
  private:
    expression_ref<E1> lhs;
    expression_ref<E2> rhs;

  public:
    static constexpr bool IS_LEAF = false;

    constexpr MatrixConcatenate(const E1& lhs_, const E2& rhs_) : lhs(lhs_), rhs(rhs_) {
    }

    constexpr R operator[](size_t row, size_t column) const {
      return (column < M1 ? lhs[row, column] : rhs[row, column - M1]);
    }
};

template <size_t N, size_t M1, size_t M2, Concepts::Ring R, class E1, class E2>
constexpr MatrixConcatenate<N, M1, M2, R, E1, E2> concatenate(
    const MatrixExpression<N, M1, R, E1>& lhs,
    const MatrixExpression<N, M2, R, E2>& rhs) {
  return MatrixConcatenate<N, M1, M2, R, E1, E2>(static_cast<const E1&>(lhs),
                                                 static_cast<const E2&>(rhs));
}

template <size_t N, size_t M, Concepts::Ring R, class E>
class MatrixScale : public MatrixExpression<N, M, R, MatrixScale<N, M, R, E>> {
  private:
    expression_ref<E> expression;
    R scale;

  public:
    static constexpr bool IS_LEAF = false;

    constexpr MatrixScale(const E& expression_, const R& scale_)
        : expression(expression_), scale(scale_) {
    }

    constexpr R operator[](size_t row, size_t column) const {
      return expression[row, column] * scale;
    }
};

template <size_t N, size_t M, Concepts::Ring R, class E>
constexpr MatrixScale<N, M, R, E> operator*(const MatrixExpression<N, M, R, E>& expression,
                                            const R& scale) {
  return MatrixScale<N, M, R, E>(static_cast<const E&>(expression), scale);
}

template <size_t N, size_t M, Concepts::Ring R, class E>
constexpr MatrixScale<N, M, R, E> operator*(const R& scale,
                                            const MatrixExpression<N, M, R, E>& expression) {
  return MatrixScale<N, M, R, E>(static_cast<const E&>(expression), scale);
}

template <size_t N, Concepts::Ring R>
  requires Concepts::HasOne<R>
struct Concepts::WithOne<Matrix<N, N, R>> {
    static inline UnaryMatrix<N, N, R> one() {
      return UnaryMatrix<N, N, R>();
    }
};

template <size_t N, Concepts::Field F>
struct Concepts::Invertible<Matrix<N, N, F>> {
    static inline Matrix<N, N, F> invert(const Matrix<N, N, F>& matrix) {
      return matrix.inverted();
    }
};

template <size_t N, size_t M, Concepts::Ring R, typename E>
inline std::ostream& operator<<(std::ostream& out, const MatrixExpression<N, M, R, E>& matrix) {
  for (size_t i = 0; i < N; ++i) {
    out << (i == 0 ? "{(" : " (");
    for (size_t j = 0; j < M; ++j) {
      out << matrix[i, j];
      if (j + 1 < M) {
        out << " ";
      }
    }
    out << ")";
    if (i + 1 < N) {
      out << "\n";
    }
  }
  return out << "}";
}
}  // namespace Math

using BigInteger = Math::BigInteger;
using Rational = Math::Rational;

template <size_t N>
using Residue = Math::Residue<N>;

template <size_t N, size_t M, typename Field = Rational>
using Matrix = Math::Matrix<N, M, Field>;

template <size_t N, typename Field = Rational>
using SquareMatrix = Math::Matrix<N, N, Field>;

#define CPP23 1

#endif
