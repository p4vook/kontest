#ifndef __STRING_H
#define __STRING_H

#include <cstring>
#include <ios>
#include <istream>
#include <limits>
#include <memory>
#include <ostream>

template <typename CharT>
struct CharTraits {
    using char_type = CharT;
    using int_type = int;
    using pos_type = std::size_t;

    static void assign(char_type& c1, const char_type& c2) noexcept;

    static char_type* assign(char_type* ptr, size_t count, char_type ch) noexcept;

    static char_type* move(char_type* dest, const char_type* src, size_t count) noexcept;

    static char_type* copy(char_type* dest, const char_type* src, size_t count) noexcept;

    static bool lt(char_type lhs, char_type rhs) noexcept;

    static bool eq(char_type lhs, char_type rhs) noexcept;

    static int compare(const char_type* lhs, const char_type* rhs, size_t count) noexcept;

    static size_t length(const char_type* ptr) noexcept;

    static size_t find(const char_type* ptr, size_t cnt, char_type c) noexcept;

    static int_type to_int_type(char_type ch) noexcept;

    static bool eq_int_type(int_type c1, int_type c2);
};

template <>
struct CharTraits<char> {
    using char_type = char;
    using int_type = int;
    using pos_type = std::size_t;

    static inline constexpr void assign(char& c1, const char& c2) noexcept {
      c1 = c2;
    }

    static inline char* assign(char* ptr, size_t count, char ch) noexcept {
      std::memset(ptr, ch, count);
      return ptr;
    }

    static inline char* move(char* dest, const char* src, size_t count) noexcept {
      std::memmove(dest, src, count);
      return dest;
    }

    static inline char* copy(char* dest, const char* src, size_t count) noexcept {
      std::memcpy(dest, src, count);
      return dest;
    }

    static inline constexpr bool lt(char lhs, char rhs) noexcept {
      return (unsigned char)lhs < (unsigned char)rhs;
    }

    static inline constexpr bool eq(char lhs, char rhs) noexcept {
      return lhs == rhs;
    }

    static int compare(const char* lhs, const char* rhs, size_t count) noexcept {
      return std::memcmp(lhs, rhs, count);
    }

    static inline size_t length(const char* ptr) noexcept {
      return strlen(ptr);
    }

    static const char* find(const char* ptr, char ch, size_t count) noexcept {
      if (count == 0) {
        return nullptr;
      }
      return reinterpret_cast<const char*>(std::memchr(ptr, ch, count));  // NOLINT need for speed
    }

    static inline constexpr int to_int_type(char ch) noexcept {
      return static_cast<int>(static_cast<unsigned char>(ch));
    }

    static inline constexpr bool eq_int_type(int c1, int c2) noexcept {
      return c1 == c2;
    }
};

template <typename CharT, typename Traits = CharTraits<CharT>>
class BasicString {
  private:
    const static size_t REALLOCATE_SCALE = 2;
    size_t capacity_{};
    CharT* buffer;
    size_t size_{};

    inline void allocate(size_t new_capacity) {
      capacity_ = new_capacity;
      buffer = reinterpret_cast<CharT*>(
          malloc(sizeof(CharT) * new_capacity));  // NOLINT C-style allocation
    }

    inline void reallocate(size_t new_capacity) {
      if (new_capacity < size_ + 1) {
        size_ = new_capacity - 1;
      }
      if (new_capacity == capacity_) {
        return;
      }
      buffer = reinterpret_cast<CharT*>(
          realloc(buffer, sizeof(CharT) * new_capacity));  // NOLINT C-style allocation
      capacity_ = new_capacity;
    }

    inline void reallocate_to_fit(size_t new_size) {
      if (new_size < capacity_) {
        return;
      }
      size_t new_capacity = capacity_;
      while (new_size >= new_capacity) {
        new_capacity *= REALLOCATE_SCALE;
      }
      reallocate(new_capacity);
    }

  public:
    BasicString(const CharT* cstr) {
      size_t new_size = Traits::length(cstr);
      allocate(new_size + 1);
      size_ = new_size;
      Traits::copy(buffer, cstr, size_ + 1);
    }

    BasicString(size_t length, CharT fill) : size_(length) {
      allocate(length + 1);
      Traits::assign(buffer, length, fill);
      Traits::assign(buffer[size_], CharT());
    }

    BasicString() noexcept {
      allocate(1);
      Traits::assign(buffer[0], CharT());
    }

    BasicString(const BasicString& other) : size_(other.size()) {
      allocate(other.size() + 1);
      Traits::copy(buffer, other.buffer, other.size() + 1);
    }

    BasicString(BasicString&& other) noexcept : BasicString() {
      std::swap(buffer, other.buffer);
      std::swap(capacity_, other.capacity_);
      std::swap(size_, other.size_);
    }

    inline BasicString& operator=(const BasicString& other) {
      if (this != std::addressof(other)) {
        reallocate_to_fit(other.size());
        Traits::copy(buffer, other.buffer, other.size() + 1);
        size_ = other.size();
      }
      return *this;
    }

    inline int compare(const BasicString& other) const {
      int prefix_order = Traits::compare(buffer, other.buffer, std::min(size(), other.size()));
      if (prefix_order != 0) {
        return prefix_order;
      }
      return (size() < other.size() ? -1 : (size() == other.size() ? 0 : 1));
    }

    inline int compare(const CharT* other) const {
      size_t other_len = Traits::length(other);
      int prefix_order = Traits::compare(buffer, other, std::min(size(), other_len));
      if (prefix_order != 0) {
        return prefix_order;
      }
      return (size() < other_len ? -1 : (size() == other_len ? 0 : 1));
    }

    CharT& operator[](size_t pos) noexcept {
      return buffer[pos];
    }

    const CharT& operator[](size_t pos) const noexcept {
      return buffer[pos];
    }

    inline void push_back(CharT chr) {
      reallocate_to_fit(size_ + 1);
      Traits::assign(buffer[size_++], chr);
      Traits::assign(buffer[size_], CharT());
    }

    inline void pop_back() {
      Traits::assign(buffer[--size_], CharT());
    }

    CharT& front() noexcept {
      return buffer[0];
    }

    const CharT& front() const noexcept {
      return buffer[0];
    }

    CharT& back() noexcept {
      return buffer[size_ - 1];
    }

    const CharT& back() const noexcept {
      return buffer[size_ - 1];
    }

    inline size_t length() const noexcept {
      return size_;
    }

    inline size_t size() const noexcept {
      return size_;
    }

    inline size_t capacity() const noexcept {
      return capacity_ - 1;
    }

    inline BasicString& operator+=(const BasicString& other) {
      size_t new_size = size() + other.size();
      reallocate_to_fit(new_size);
      Traits::move(buffer + size(), other.buffer, other.size() + 1);
      size_ = new_size;
      return (*this);
    }

    inline BasicString& operator+=(CharT chr) {
      push_back(chr);
      return (*this);
    }

    inline size_t find(const BasicString& substring) const noexcept {
      for (size_t i = 0; i + substring.size() <= size(); ++i) {
        if (Traits::compare(substring.buffer, buffer + i, substring.size()) == 0) {
          return i;
        }
      }
      return size();
    }

    inline size_t rfind(const BasicString& substring) const noexcept {
      if (substring.size() > size()) {
        return size();
      }
      for (size_t i = size() - substring.size(); i > 0; --i) {
        if (Traits::compare(substring.buffer, buffer + i, substring.size()) == 0) {
          return i;
        }
      }
      return (Traits::compare(substring.buffer, buffer, substring.size()) == 0 ? 0 : size());
    }

    inline BasicString substr(size_t start, size_t count) const {
      if (start + count > size()) {
        count = size() - start;
      }
      BasicString res;
      res.reallocate(count + 1);
      Traits::copy(res.buffer, buffer + start, count);
      res.size_ = count;
      Traits::assign(res.buffer[count], CharT());
      return res;
    }

    inline bool empty() const noexcept {
      return size() == 0;
    }

    void clear() noexcept {
      size_ = 0;
      Traits::assign(buffer[0], CharT());
    }

    inline void shrink_to_fit() {
      reallocate(size() + 1);
    }

    char* data() noexcept {
      return buffer;
    }

    const char* data() const noexcept {
      return buffer;
    }

    inline ~BasicString() noexcept {
      free(buffer);
    }
};

template <typename CharT, typename Traits>
inline bool operator==(const BasicString<CharT, Traits>& lhs,
                       const BasicString<CharT, Traits>& rhs) noexcept {
  return lhs.size() == rhs.size() && Traits::compare(lhs.data(), rhs.data(), lhs.size()) == 0;
}

template <typename CharT, typename Traits>
inline bool operator==(const BasicString<CharT, Traits>& lhs, const CharT* rhs) noexcept {
  size_t rhs_len = Traits::length(rhs);
  return lhs.size() == rhs_len && Traits::compare(lhs.data(), rhs, rhs_len) == 0;
}

template <typename CharT, typename Traits>
inline bool operator==(const CharT* lhs, const BasicString<CharT, Traits>& rhs) noexcept {
  return rhs == lhs;
}

template <typename CharT, typename Traits>
inline bool operator<(const BasicString<CharT, Traits>& lhs,
                      const BasicString<CharT, Traits>& rhs) noexcept {
  return lhs.compare(rhs) < 0;
}

template <typename CharT, typename Traits>
inline bool operator<(const BasicString<CharT, Traits>& lhs, const CharT* rhs) noexcept {
  return lhs.compare(rhs) < 0;
}

template <typename CharT, typename Traits>
inline bool operator<(const CharT* lhs, const BasicString<CharT, Traits>& rhs) noexcept {
  return rhs.compare(lhs) > 0;
}

template <typename CharT, typename Traits>
inline bool operator>(const BasicString<CharT, Traits>& lhs,
                      const BasicString<CharT, Traits>& rhs) noexcept {
  return lhs.compare(rhs) > 0;
}

template <typename CharT, typename Traits>
inline bool operator>(const BasicString<CharT, Traits>& lhs, const CharT* rhs) noexcept {
  return lhs.compare(rhs) > 0;
}

template <typename CharT, typename Traits>
inline bool operator>(const CharT* lhs, const BasicString<CharT, Traits>& rhs) noexcept {
  return rhs.compare(lhs) < 0;
}

template <typename CharT, typename Traits>
inline bool operator<=(const BasicString<CharT, Traits>& lhs,
                       const BasicString<CharT, Traits>& rhs) noexcept {
  return lhs.compare(rhs) <= 0;
}

template <typename CharT, typename Traits>
inline bool operator<=(const BasicString<CharT, Traits>& lhs, const CharT* rhs) noexcept {
  return lhs.compare(rhs) <= 0;
}

template <typename CharT, typename Traits>
inline bool operator<=(const CharT* lhs, const BasicString<CharT, Traits>& rhs) noexcept {
  return rhs.compare(lhs) >= 0;
}

template <typename CharT, typename Traits>
inline bool operator>=(const BasicString<CharT, Traits>& lhs,
                       const BasicString<CharT, Traits>& rhs) noexcept {
  return lhs.compare(rhs) >= 0;
}

template <typename CharT, typename Traits>
inline bool operator>=(const BasicString<CharT, Traits>& lhs, const CharT* rhs) noexcept {
  return lhs.compare(rhs) >= 0;
}

template <typename CharT, typename Traits>
inline bool operator>=(const CharT* lhs, const BasicString<CharT, Traits>& rhs) noexcept {
  return rhs.compare(lhs) <= 0;
}

template <typename CharT, typename Traits>
inline bool operator!=(const BasicString<CharT, Traits>& lhs,
                       const BasicString<CharT, Traits>& rhs) noexcept {
  return !(lhs == rhs);
}

template <typename CharT, typename Traits>
inline bool operator!=(const CharT* lhs, const BasicString<CharT, Traits>& rhs) noexcept {
  return !(lhs == rhs);
}

template <typename CharT, typename Traits>
inline bool operator!=(const BasicString<CharT, Traits>& lhs, const CharT* rhs) noexcept {
  return !(lhs == rhs);
}

template <typename CharT, typename Traits>
BasicString<CharT, Traits> operator+(const BasicString<CharT, Traits>& lhs,
                                     const BasicString<CharT, Traits>& rhs) {
  BasicString<CharT, Traits> res = lhs;
  res += rhs;
  return res;
}

template <typename CharT, typename Traits>
BasicString<CharT, Traits> operator+(const BasicString<CharT, Traits>& lhs, CharT rhs) {
  BasicString<CharT, Traits> res = lhs;
  res += rhs;
  return res;
}

template <typename CharT, typename Traits>
BasicString<CharT, Traits> operator+(CharT lhs, const BasicString<CharT, Traits>& rhs) {
  BasicString<CharT, Traits> res(1, lhs);
  res += rhs;
  return res;
}

template <typename CharT, typename Traits>
BasicString<CharT, Traits> operator+(BasicString<CharT, Traits>&& lhs,
                                     const BasicString<CharT, Traits>& rhs) {
  lhs += rhs;
  return std::move(lhs);
}

template <typename CharT, typename Traits>
inline BasicString<CharT, Traits> operator+(BasicString<CharT, Traits>&& lhs, CharT rhs) {
  lhs.push_back(rhs);
  return std::move(lhs);
}

template <typename CharT, typename StringTraits, typename StreamTraits>
std::basic_ostream<CharT, StreamTraits>& operator<<(
    std::basic_ostream<CharT, StreamTraits>& ostream,
    const BasicString<CharT, StringTraits>& str) {
  typename std::basic_ostream<CharT, StreamTraits>::sentry sen(ostream);
  if (sen) {
    ostream.write(str.data(), static_cast<std::streamsize>(str.size()));
  }
  return ostream;
}

template <typename CharT, typename StringTraits, typename StreamTraits>
std::basic_istream<CharT, StreamTraits>& operator>>(
    std::basic_istream<CharT, StreamTraits>& istream,
    BasicString<CharT, StringTraits>& str) {
  typename std::basic_istream<CharT, StreamTraits>::sentry sen(istream);
  if (sen) {
    CharT next_char;
    std::streamsize max_count = istream.width();
    if (max_count <= 0)
      max_count = std::numeric_limits<std::streamsize>::max();
    std::streamsize count = 0;
    const std::ctype<CharT>& ctype = std::use_facet<std::ctype<CharT>>(istream.getloc());
    while (count < max_count) {
      typename StreamTraits::int_type int_value = istream.rdbuf()->sgetc();
      if (StreamTraits::eq_int_type(int_value, StreamTraits::eof())) {
        istream.setstate(std::ios_base::badbit);
        break;
      }
      next_char = StreamTraits::to_char_type(int_value);
      if (ctype.is(ctype.space, next_char))
        break;
      str.push_back(next_char);
      ++count;
      istream.rdbuf()->sbumpc();
    }
    istream.width(0);
    if (count == 0) {
      istream.setstate(std::ios_base::failbit);
    }
  }
  return istream;
}

using String = BasicString<char>;
#endif  // __STRING_H
