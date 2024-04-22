#ifndef LIST_H
#define LIST_H

#include <iterator>
#include <memory>
#include <span>

template <size_t Extent = std::dynamic_extent>
class StackStorage {};

template <>
class StackStorage<std::dynamic_extent> {
    using span_type = std::span<char, std::dynamic_extent>;

  private:
    span_type buffer;
    span_type::size_type current_size{};

  public:
    template <typename T, size_t E>
    friend class StackAllocator;

    StackStorage() = delete;

    constexpr StackStorage(span_type buffer_) noexcept : buffer(buffer_) {
    }

    StackStorage(const StackStorage<std::dynamic_extent>& other) = delete;

    StackStorage& operator=(const StackStorage<std::dynamic_extent>& other) = delete;

    StackStorage(StackStorage<std::dynamic_extent>&& other) = delete;

    StackStorage& operator=(StackStorage<std::dynamic_extent>&& other) = delete;

    constexpr ~StackStorage() noexcept = default;
};

template <size_t Extent>
  requires(Extent != std::dynamic_extent)
class StackStorage<Extent> {
    using span_type = std::span<char, Extent>;
    span_type::size_type current_size{};

    template <typename T, size_t E>
    friend class StackAllocator;

  private:
    std::array<char, Extent> data;
    span_type buffer;

  public:
    StackStorage() noexcept : buffer(data) {
    }

    StackStorage(const StackStorage<Extent>& other) = delete;

    StackStorage& operator=(const StackStorage<Extent>& other) = delete;

    StackStorage(StackStorage<Extent>&& other) = delete;

    StackStorage& operator=(StackStorage<Extent>&& other) = delete;

    constexpr ~StackStorage() noexcept = default;
};

template <typename T, size_t Extent>
class StackAllocator {
  private:
    using span_type = StackStorage<Extent>::span_type;

    StackStorage<Extent>* storage;

    template <typename U, size_t E>
    friend class StackAllocator;

  public:
    using value_type = T;
    using size_type = span_type::size_type;
    using pointer = T*;
    using difference_type = span_type::difference_type;

    const static bool is_always_equal = false;

    constexpr void align() noexcept {
      if (size_type mod =
              reinterpret_cast<uintptr_t>(&storage->buffer[storage->current_size]) % alignof(T);
          mod != 0) {
        storage->current_size += alignof(T) - mod;
      }
    }

    constexpr StackAllocator(StackStorage<Extent>& storage_) noexcept : storage(&storage_) {
    }

    template <typename U>
    constexpr StackAllocator(const StackAllocator<U, Extent>& alloc_a) : storage(alloc_a.storage) {
      if (storage->current_size > storage->buffer.size()) {
        throw std::bad_alloc{};
      }
    }

    constexpr pointer allocate(size_type count) {
      align();
      if (storage->current_size + count * sizeof(T) > storage->buffer.size()) {
        throw std::bad_alloc{};
      }
      auto res = reinterpret_cast<pointer>(&storage->buffer[storage->current_size]);
      storage->current_size += count * sizeof(T);
      return res;
    }

    constexpr void deallocate(pointer ptr [[maybe_unused]],
                              size_type count [[maybe_unused]]) noexcept {
      // do nothing
    }

    constexpr friend bool operator==(const StackAllocator<T, Extent>& lhs,
                                     const StackAllocator<T, Extent>& rhs) {
      return lhs.storage == rhs.storage;
    }

    template <typename U>
    struct rebind {
        using other = StackAllocator<U, Extent>;
    };
};

template <typename T, typename Allocator = std::allocator<T>>
struct List {
  private:
    struct BaseNode {
        BaseNode* previous;
        BaseNode* next;
    };

    struct Node : BaseNode {
        T value;

        Node(const T& value_) : value(value_) {
        }

        template <typename... Args>
        Node(Args&&... args) : value(args...) {
        }
    };

    struct FakeNode : BaseNode {
        FakeNode() {
          auto base_node = static_cast<BaseNode*>(this);
          base_node->next = base_node;
          base_node->previous = base_node;
        }
    };

    template <bool IsConst>
    struct BaseIterator {
      public:
        using iterator_category = std::bidirectional_iterator_tag;
        using difference_type = ssize_t;
        using value_type = T;
        using pointer = std::conditional_t<IsConst, const T*, T*>;
        using reference = std::conditional_t<IsConst, const T&, T&>;

      private:
        using node_type = std::conditional_t<IsConst, const Node*, Node*>;

        BaseNode* base_node;
        friend struct List;

      public:
        constexpr BaseIterator(BaseNode* node) : base_node(node) {
        }

        constexpr BaseIterator(const BaseIterator<false>& iterator)
          requires(IsConst)
            : BaseIterator(iterator.base_node) {
        }

        constexpr BaseIterator(const BaseIterator<IsConst>& iterator) = default;

        constexpr BaseIterator& operator=(const BaseIterator<false>& iterator)
          requires(IsConst)
        {
          base_node = iterator.base_node;
          return *this;
        }

        constexpr BaseIterator& operator=(const BaseIterator<IsConst>& iterator) = default;

        constexpr reference operator*() const {
          return static_cast<node_type>(base_node)->value;
        }

        constexpr pointer operator->() const {
          return &(*(*this));
        }

        constexpr BaseIterator& operator++() {
          base_node = base_node->next;
          return *this;
        }

        constexpr BaseIterator& operator--() {
          base_node = base_node->previous;
          return *this;
        }

        constexpr BaseIterator operator++(int) {
          BaseIterator tmp = *this;
          ++*this;
          return tmp;
        }

        constexpr BaseIterator operator--(int) {
          BaseIterator tmp = *this;
          --*this;
          return tmp;
        }

        constexpr friend bool operator==(const BaseIterator<IsConst>& lhs,
                                         const BaseIterator<IsConst>& rhs) {
          return lhs.base_node == rhs.base_node;
        }
    };

    FakeNode root;
    size_t size_ = 0;

    using node_alloc = std::allocator_traits<Allocator>::template rebind_alloc<Node>;
    using node_traits = std::allocator_traits<node_alloc>;
    [[no_unique_address]] node_alloc alloc;

  public:
    using iterator = BaseIterator<false>;
    using const_iterator = BaseIterator<true>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    List() = default;

    [[nodiscard]] size_t size() const {
      return size_;
    }

  private:
    void link(const_iterator pos, BaseNode* node) {
      node->previous = pos.base_node->previous;
      node->next = pos.base_node;
      node->previous->next = node;
      node->next->previous = node;
      ++size_;
    }

  public:
    iterator insert(const_iterator pos, const T& value) {
      return emplace(pos, value);
    }

    template <typename... Args>
    iterator emplace(const_iterator pos, Args&&... args) {
      Node* emplaced = nullptr;
      emplaced = node_traits::allocate(alloc, 1);
      try {
        node_traits::construct(alloc, emplaced, std::forward<Args>(args)...);
      } catch (...) {
        node_traits::deallocate(alloc, emplaced, 1);
        throw;
      }

      auto emplaced_base = static_cast<BaseNode*>(emplaced);
      link(pos, emplaced_base);
      return emplaced;
    }

    iterator erase(const_iterator pos) noexcept {
      pos.base_node->previous->next = pos.base_node->next;
      pos.base_node->next->previous = pos.base_node->previous;

      auto result = pos.base_node->next;
      auto node_ptr = static_cast<Node*>(pos.base_node);
      node_traits::destroy(alloc, node_ptr);
      node_traits::deallocate(alloc, node_ptr, 1);
      --size_;
      return result;
    }

    iterator begin() {
      return static_cast<BaseNode&>(root).next;
    }

    iterator end() {
      return static_cast<BaseNode*>(&root);
    }

    const_iterator begin() const {
      return static_cast<const BaseNode&>(root).next;
    }

    const_iterator end() const {
      return const_cast<BaseNode*>(static_cast<const BaseNode*>(&root));
    }

    const_iterator cbegin() const {
      return begin();
    }

    const_iterator cend() const {
      return end();
    }

    reverse_iterator rbegin() {
      return reverse_iterator(end());
    }

    reverse_iterator rend() {
      return reverse_iterator(begin());
    }

    const_reverse_iterator rbegin() const {
      return const_reverse_iterator(end());
    }

    const_reverse_iterator rend() const {
      return const_reverse_iterator(begin());
    }

    const_reverse_iterator crbegin() const {
      return rbegin();
    }

    const_reverse_iterator crend() const {
      return rend();
    }

    void pop_back() noexcept {
      erase(std::prev(end()));
    }

    void pop_front() noexcept {
      erase(begin());
    }

    void push_back(const T& value) {
      insert(end(), value);
    }

    template <typename... Args>
    void emplace_back(Args&&... args) {
      emplace(end(), args...);
    }

    void push_front(const T& value) {
      insert(begin(), value);
    }

    template <typename... Args>
    void emplace_front(Args&&... args) {
      emplace(begin(), args...);
    }

    [[nodiscard]] bool empty() const {
      return size() == 0;
    }

    void clear() noexcept {
      while (!empty()) {
        pop_front();
      }
    }

    Allocator get_allocator() const {
      return alloc;
    }

    List(size_t count, const T& value) : List(count, value, Allocator{}) {
    }

    List(size_t count) : List(count, Allocator{}) {
    }

    List(const Allocator& alloc_) : alloc(alloc_) {
    }

    List(size_t count, const T& value, const Allocator& alloc_) : List(alloc_) {
      while (size() < count) {
        push_back(value);
      }
    }

    List(size_t count, const Allocator& alloc_) : List(alloc_) {
      while (size() < count) {
        emplace_back();
      }
    }

    List(const List& other)
        : List(node_traits::select_on_container_copy_construction(other.alloc)) {
      for (const T& val : other) {
        push_back(val);
      }
    }

    List& operator=(const List& other) {
      if (this != std::addressof(other)) {
        if constexpr (node_traits::propagate_on_container_copy_assignment::value) {
          if (alloc != other.alloc) {
            clear();
          }
          alloc = other.alloc;
        }
        iterator it = begin();
        iterator e = end();
        const_iterator first = other.begin();
        const_iterator last = other.end();
        for (; first != last && it != e; ++first, static_cast<void>(++it)) {
          *it = *first;
        }
        if (it == e) {
          try {
            for (; first != last; ++first, static_cast<void>(++it)) {
              it = insert(it, *first);
            }
          } catch (...) {
            const_iterator b = other.begin();
            for (; first != b; --first) {
              pop_back();
            }
            throw;
          }
        } else {
          for (; it != e;) {
            it = erase(it);
          }
        }
      }
      return *this;
    }

    ~List() {
      clear();
    }
};

#endif  // LIST_H
