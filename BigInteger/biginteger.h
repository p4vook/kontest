#ifndef BIGINTEGER_H
#define BIGINTEGER_H

#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <istream>
#include <iterator>
#include <numeric>
#include <ostream>

class BigInteger {
  private:
    using block_type = uint32_t;

    bool isNegative;
    static const int64_t MOD = 1'000'000'000;
    // static const int64_t MOD = 10;
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
          result.push_back('0' + block % MOD_BASE);
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

inline BigInteger operator+(const BigInteger& lhs, const BigInteger& rhs) {
  BigInteger result = lhs;
  return result += rhs;
}

inline BigInteger& BigInteger::operator-=(const BigInteger& rhs) {
  return *this += -rhs;
}

inline BigInteger operator-(const BigInteger& lhs, const BigInteger& rhs) {
  BigInteger result = lhs;
  return result -= rhs;
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
    in >> str;
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
    BigInteger nominator = 0;
    BigInteger denominator = 1;

  public:
    Rational() = default;

    Rational(int x) : nominator(x), denominator(1) {
    }

    Rational(BigInteger x) : nominator(std::move(x)), denominator(1) {
    }

    Rational(BigInteger nominator, BigInteger denominator)
        : nominator(std::move(nominator)), denominator(std::move(denominator)) {
    }

    inline void normalizeDenominator() {
      if (denominator.negative()) {
        nominator.negate();
        denominator.negate();
      }
    }

    inline void normalize() {
      normalizeDenominator();
      BigInteger divisor = gcd(nominator, denominator);
      nominator /= divisor;
      denominator /= divisor;
    }

    inline void invert() {
      std::swap(nominator, denominator);
      normalizeDenominator();
    }

    inline Rational& operator+=(const Rational& other) & {
      nominator *= other.denominator;
      nominator += other.nominator * denominator;
      denominator *= other.denominator;
      return *this;
    }

    inline Rational operator-() const {
      Rational result = *this;
      result.nominator.negate();
      return result;
    }

    inline Rational& operator-=(const Rational& other) & {
      return (*this) += (-other);
    }

    inline Rational& operator*=(const Rational& other) & {
      nominator *= other.nominator;
      denominator *= other.denominator;
      return *this;
    }

    inline Rational& operator/=(const Rational& other) & {
      nominator *= other.denominator;
      denominator *= other.nominator;
      normalizeDenominator();
      return *this;
    }

    inline bool operator==(const Rational& other) const& {
      return nominator * other.denominator == other.nominator * denominator;
    }

    inline std::strong_ordering operator<=>(const Rational& other) const& {
      return nominator * other.denominator <=> other.nominator * denominator;
    }

    inline std::string toString() {
      normalize();
      std::string result = nominator.toString();
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
      BigInteger temp = nominator;
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
      if (nominator.negative()) {
        result.insert(result.begin(), '-');
      }
      return result;
    }

    inline std::string asDecimal(size_t precision) const {
      Rational temp = *this;
      return temp.asDecimal(precision);
    }

    inline explicit operator double() const {
      return std::stod(asDecimal(16));
    }
};

inline Rational operator+(const Rational& lhs, const Rational& rhs) {
  Rational result = lhs;
  return result += rhs;
}

inline Rational operator-(const Rational& lhs, const Rational& rhs) {
  Rational result = lhs;
  return result -= rhs;
}

inline Rational operator*(const Rational& lhs, const Rational& rhs) {
  Rational result = lhs;
  return result *= rhs;
}

inline Rational operator/(const Rational& lhs, const Rational& rhs) {
  Rational result = lhs;
  return result /= rhs;
}

#endif  // BIGINTEGER_H
