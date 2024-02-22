#ifndef DEQUE_H
#define DEQUE_H
#include <cstdlib>
#include <iterator>
#include <optional>
#include <vector>

template <typename T>
class Deque {
  private:
    class Chunk {
      public:
        constexpr static const size_t CHUNK_SIZE = 32;

        T* raw;
        size_t start{};
        size_t end{};

        Chunk() : raw(static_cast<T*>(malloc(sizeof(T) * CHUNK_SIZE))) {
          if (!raw) {
            throw std::bad_alloc{};
          }
        }

        Chunk(const Chunk& other) = delete;
        Chunk& operator=(const Chunk& other) = delete;

        Chunk(Chunk&& other) noexcept : raw(other.raw), start(other.start), end(other.end) {
          other.raw = nullptr;
        }

        Chunk& operator=(Chunk&& other) = default;

        void swap(Chunk& other) {
          std::swap(raw, other.raw);
          std::swap(start, other.start);
          std::swap(end, other.end);
        }

        ~Chunk() {
          if (!raw) {
            return;
          }
          for (size_t i = start; i < end; ++i) {
            (raw + i)->~T();
          }
          free(raw);
        }
    };

    std::pair<size_t, size_t> start_pos{0, 0};
    std::pair<size_t, size_t> end_pos{0, 0};

    std::vector<Chunk> chunks;

    template <bool IsConst>
    class IteratorBase {
      public:
        using vector_iterator_t = std::conditional_t<IsConst,
                                                     typename std::vector<Chunk>::const_iterator,
                                                     typename std::vector<Chunk>::iterator>;

        using iterator_category = std::random_access_iterator_tag;
        using difference_type = ssize_t;
        using value_type = T;
        using pointer = std::conditional_t<IsConst, const T*, T*>;
        using reference = std::conditional_t<IsConst, const T&, T&>;

      private:
        vector_iterator_t chunk_iterator;
        ssize_t chunk_pos;

        IteratorBase& next() {
          if (++chunk_pos == Chunk::CHUNK_SIZE) {
            ++chunk_iterator;
            chunk_pos = 0;
          }
          return *this;
        }

        IteratorBase& prev() {
          if (chunk_pos-- == 0) {
            --chunk_iterator;
            chunk_pos = Chunk::CHUNK_SIZE - 1;
          }
          return *this;
        }

        IteratorBase& shift_left(difference_type delta) {
          if (delta < 0) {
            return shift_right(-delta);
          }
          chunk_iterator -= delta / Chunk::CHUNK_SIZE;
          difference_type chunk_delta = delta % Chunk::CHUNK_SIZE;
          chunk_pos -= chunk_delta;
          if (chunk_pos < 0) {
            --chunk_iterator;
            chunk_pos += Chunk::CHUNK_SIZE;
          }
          return *this;
        }

        IteratorBase& shift_right(difference_type delta) {
          if (delta < 0) {
            return shift_left(-delta);
          }
          chunk_iterator += delta / Chunk::CHUNK_SIZE;
          difference_type chunk_delta = delta % Chunk::CHUNK_SIZE;
          chunk_pos += chunk_delta;
          if (chunk_pos >= static_cast<ssize_t>(Chunk::CHUNK_SIZE)) {
            ++chunk_iterator;
            chunk_pos -= Chunk::CHUNK_SIZE;
          }
          return *this;
        }

      public:
        IteratorBase(const vector_iterator_t& chunk_iterator_, size_t chunk_pos_)
            : chunk_iterator(chunk_iterator_), chunk_pos(static_cast<difference_type>(chunk_pos_)) {
          assert(chunk_pos_ < Chunk::CHUNK_SIZE);
        }

        IteratorBase(const IteratorBase<false>& iterator)
          requires(IsConst)
            : chunk_iterator(iterator.chunk_iterator), chunk_pos(iterator.chunk_pos) {
        }

        IteratorBase(const IteratorBase<IsConst>& iterator)
            : chunk_iterator(iterator.chunk_iterator), chunk_pos(iterator.chunk_pos) {
        }

        IteratorBase& operator=(const IteratorBase<IsConst>& iterator) {
          chunk_iterator = iterator.chunk_iterator;
          chunk_pos = iterator.chunk_pos;
          return *this;
        }

        IteratorBase& operator=(const IteratorBase<false>& iterator)
          requires(IsConst)
        {
          chunk_iterator = iterator.chunk_iterator;
          chunk_pos = iterator.chunk_pos;
          return *this;
        }

        reference operator*() const {
          return chunk_iterator->raw[chunk_pos];
        }

        pointer operator->() const {
          return &(chunk_iterator->raw[chunk_pos]);
        }

        IteratorBase& operator++() {
          return next();
        }

        IteratorBase& operator--() {
          return prev();
        }

        IteratorBase operator++(int) {
          IteratorBase tmp = *this;
          ++*this;
          return tmp;
        }

        IteratorBase operator--(int) {
          IteratorBase tmp = *this;
          --*this;
          return tmp;
        }

        IteratorBase& operator+=(difference_type delta) {
          return shift_right(delta);
        }

        IteratorBase& operator-=(difference_type delta) {
          return shift_left(delta);
        }

        IteratorBase operator+(difference_type delta) const {
          IteratorBase copy(*this);
          return copy += delta;
        }

        IteratorBase operator-(difference_type delta) const {
          IteratorBase copy(*this);
          return copy -= delta;
        }

        friend bool operator==(const IteratorBase<IsConst>& lhs, const IteratorBase<IsConst>& rhs) {
          return lhs.chunk_iterator == rhs.chunk_iterator && lhs.chunk_pos == rhs.chunk_pos;
        }

        friend auto operator<=>(const IteratorBase<IsConst>& lhs,
                                const IteratorBase<IsConst>& rhs) {
          return std::pair{lhs.chunk_iterator, lhs.chunk_pos} <=>
                 std::pair{rhs.chunk_iterator, rhs.chunk_pos};
        }

        friend difference_type operator-(const IteratorBase<IsConst>& lhs,
                                         const IteratorBase<IsConst>& rhs) {
          return (lhs.chunk_iterator - rhs.chunk_iterator) * Chunk::CHUNK_SIZE + lhs.chunk_pos -
                 rhs.chunk_pos;
        }
    };

    void reallocate() {
      std::vector<Chunk> new_chunks(3 * (chunks.size() + 1));
      size_t offset = chunks.size() + 1;
      for (size_t i = 0; i < chunks.size(); ++i) {
        new_chunks[offset + i].swap(chunks[i]);
      }
      chunks.swap(new_chunks);
      start_pos.first += offset;
      end_pos.first += offset;
    }

    bool reallocate_needed(ssize_t delta) {
      return (delta < 0 && start_pos.first * Chunk::CHUNK_SIZE + start_pos.second <
                               static_cast<size_t>(-delta)) ||
             (delta > 0 && end_pos.first * Chunk::CHUNK_SIZE + end_pos.second + delta >
                               chunks.size() * Chunk::CHUNK_SIZE);
    }

    void reallocate_to_fit(ssize_t delta) {
      if (reallocate_needed(delta)) {
        reallocate();
      }
    }

    static std::pair<size_t, size_t> right(std::pair<size_t, size_t> pos) {
      if (++pos.second == Chunk::CHUNK_SIZE) {
        ++pos.first;
        pos.second = 0;
      }
      return pos;
    }

    static std::pair<int, int> left(std::pair<size_t, size_t> pos) {
      if (pos.second-- == 0) {
        --pos.first;
        pos.second = Chunk::CHUNK_SIZE - 1;
      }
      return pos;
    }

  public:
    using iterator = IteratorBase<false>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_iterator = IteratorBase<true>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    Deque() = default;

    Deque(Deque&& other) noexcept
        : start_pos(other.start_pos), end_pos(other.end_pos), chunks(std::move(other.chunks)) {
    }

    Deque& operator=(Deque&& other) noexcept {
      chunks = std::move(other.chunks);
      start_pos = other.start_pos;
      end_pos = other.end_pos;
      return *this;
    }

  private:
    struct CancellationWrapper {
        Deque& deque;

        Deque old_deque;
        iterator iter;

        CancellationWrapper(const CancellationWrapper& wrap) = delete;
        CancellationWrapper& operator=(const CancellationWrapper& wrap) = delete;

        CancellationWrapper(Deque& deque_)
            : deque(deque_), old_deque(std::move(deque_)), iter(old_deque.begin()) {
        }

        ~CancellationWrapper() {
          if (iter == deque.end()) {
            return;
          }

          for (iterator it = deque.begin(); it != iter; ++it) {
            it->~T();
          }
          deque = std::move(old_deque);
        }
    };

    Deque(size_t size, size_t, size_t)
        : end_pos({size / Chunk::CHUNK_SIZE, size % Chunk::CHUNK_SIZE}),
          chunks(size / Chunk::CHUNK_SIZE + 1) {
    }

    void fill_chunk_info() {
      for (size_t chunk = start_pos.first; chunk <= end_pos.first; ++chunk) {
        chunks[chunk].start = (chunk == start_pos.first ? start_pos.second : 0);
        chunks[chunk].end = (chunk == end_pos.first ? end_pos.second : Chunk::CHUNK_SIZE);
      }
    }

    void copy(const Deque& other) {
      CancellationWrapper wrap(*this);
      new (this) Deque(other.size(), 0, 0);
      size_t i = 0;
      for (wrap.iter = begin(); wrap.iter != end(); ++wrap.iter, ++i) {
        new (&*wrap.iter) T{other[i]};
      }
      fill_chunk_info();
    }

  public:
    Deque(const Deque& other) {
      copy(other);
    }

    Deque& operator=(const Deque& other) {
      copy(other);
      return *this;
    }

    Deque(size_t size) {
      CancellationWrapper wrap(*this);
      new (this) Deque(size, 0, 0);
      for (wrap.iter = begin(); wrap.iter != end(); ++wrap.iter) {
        new (&*wrap.iter) T{};
      }
      fill_chunk_info();
    }

    Deque(size_t size, const T& filler) : chunks(size / Chunk::CHUNK_SIZE + 1) {
      CancellationWrapper wrap(*this);
      new (this) Deque(size, 0, 0);
      for (wrap.iter = begin(); wrap.iter != end(); ++wrap.iter) {
        new (&*wrap.iter) T{filler};
      }
      fill_chunk_info();
    }

  private:
    struct ReallocateWrapper {
        Deque& deque;
        std::optional<Deque> copy = std::nullopt;
        bool done = false;

        ReallocateWrapper(const ReallocateWrapper&) = delete;
        ReallocateWrapper& operator=(const ReallocateWrapper&) = delete;

        ReallocateWrapper(Deque& deque_, ssize_t delta) : deque(deque_) {
          if (deque.reallocate_needed(delta)) {
            copy = deque;
            deque.reallocate();
          }
        }

        ~ReallocateWrapper() {
          if (!done && copy) {
            deque = std::move(*copy);
          }
        }
    };

  public:
    void push_front(const T& value) {
      ReallocateWrapper wrap(*this, -1);
      auto next_start = left(start_pos);
      new (chunks[next_start.first].raw + next_start.second) T(value);
      start_pos = next_start;
      if (start_pos.second == Chunk::CHUNK_SIZE - 1) {
        chunks[start_pos.first].start = start_pos.second;
        chunks[start_pos.first].end = start_pos.second + 1;
      } else {
        --chunks[start_pos.first].start;
      }
      wrap.done = true;
    }

    void pop_front() {
      (chunks[start_pos.first].raw + start_pos.second)->~T();
      ++chunks[start_pos.first].start;
      start_pos = right(start_pos);
    }

    void push_back(const T& value) {
      ReallocateWrapper wrap(*this, +1);
      new (chunks[end_pos.first].raw + end_pos.second) T(value);
      ++chunks[end_pos.first].end;
      end_pos = right(end_pos);
      wrap.done = true;
    }

    void pop_back() {
      end_pos = left(end_pos);
      (chunks[end_pos.first].raw + end_pos.second)->~T();
      --chunks[end_pos.first].end;
    }

    size_t size() const {
      return (end_pos.first - start_pos.first) * Chunk::CHUNK_SIZE +
             static_cast<ssize_t>(end_pos.second - start_pos.second);
    }

    iterator begin() {
      return iterator(chunks.begin() + start_pos.first, start_pos.second);
    }

    iterator end() {
      return iterator(chunks.begin() + end_pos.first, end_pos.second);
    }

    const_iterator begin() const {
      return cbegin();
    }

    const_iterator end() const {
      return cend();
    }

    const_iterator cbegin() const {
      return const_iterator(chunks.cbegin() + start_pos.first, start_pos.second);
    }

    const_iterator cend() const {
      return const_iterator(chunks.cbegin() + end_pos.first, end_pos.second);
    }

    reverse_iterator rbegin() {
      return reverse_iterator(end());
    }

    reverse_iterator rend() {
      return reverse_iterator(begin());
    }

    const_reverse_iterator rbegin() const {
      return crbegin();
    }

    const_reverse_iterator rend() const {
      return crend();
    }

    const_reverse_iterator crbegin() const {
      return reverse_iterator(cbegin());
    }

    const_reverse_iterator crend() const {
      return reverse_iterator(cend());
    }

    T& operator[](size_t pos) {
      return *(begin() + pos);
    }

    const T& operator[](size_t pos) const {
      return *(cbegin() + pos);
    }

    T& at(size_t pos) {
      if (pos >= size()) {
        throw std::out_of_range{"bad position"};
      }
      return (*this)[pos];
    }

    const T& at(size_t pos) const {
      if (pos >= size()) {
        throw std::out_of_range{"bad position"};
      }
      return (*this)[pos];
    }

    void insert(iterator iter, const T& element) {
      ssize_t delta = iter - begin();
      push_back(element);
      for (iterator it = --end(); it != begin() + delta; --it) {
        std::swap(*it, *std::prev(it));
      }
    }

    void erase(iterator iter) {
      std::swap(*iter, *--end());
      pop_back();
      for (iterator it = iter; it != end(); ++it) {
        std::swap(*it, *std::next(it));
      }
    }
};

#endif  // DEQUE_H