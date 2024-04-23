#ifndef __SHARED_PTR_H
#define __SHARED_PTR_H

#include <array>
#include <concepts>
#include <cstddef>
#include <cstdio>
#include <memory>

template <typename From, typename To>
concept points_as = std::convertible_to<From*, To*>;

// NOLINTNEXTLINE(cppcoreguidelines-virtual-class-destructor)
struct CountControlBlock {  // NOLINT(cppcoreguidelines-special-member-*)
  public:
    size_t count = 0;       // NOLINT(misc-non-private-member-*)
    size_t weak_count = 0;  // NOLINT(misc-non-private-member-*)

    virtual void destroy_object() noexcept = 0;
    virtual void destroy_self() noexcept = 0;

  protected:
    virtual ~CountControlBlock() = default;
};

template <typename T>
class WeakPtr;

template <typename T>
class SharedPtr {
  private:
    template <class Alloc>
    class ObjectControlBlock : public CountControlBlock {
      private:
        using T_Alloc = std::allocator_traits<Alloc>::template rebind_alloc<T>;
        using T_Traits = std::allocator_traits<T_Alloc>;

        alignas(T) std::array<char, sizeof(T)> buffer;
        [[no_unique_address]] T_Alloc alloc;

      public:
        inline T* ptr() noexcept {
          // NOLINTNEXTLINE(cppcoreguidelines-pro-type-reinterpret-cast)
          return reinterpret_cast<T*>(buffer.data());
        }

        template <typename... Args>
        inline ObjectControlBlock(Alloc alloc, Args&&... args)
            : CountControlBlock(), alloc(std::move(alloc)) {
          ::new (ptr()) T(std::forward<Args>(args)...);
          // T_Traits::construct(alloc, ptr(), std::forward<Args>(args)...);
        }

        void destroy_object() noexcept override {
          T_Traits::destroy(alloc, ptr());
        }

        void destroy_self() noexcept override {
          using SelfAlloc = std::allocator_traits<Alloc>::template rebind_alloc<ObjectControlBlock>;
          using SelfTraits = std::allocator_traits<SelfAlloc>;
          using SelfPointer = std::pointer_traits<typename SelfTraits::pointer>;

          SelfAlloc self_alloc(alloc);
          SelfTraits::deallocate(self_alloc, SelfPointer::pointer_to(*this), 1);
        }
    };

    template <class Y, class Deleter, class Alloc>
    class PointerControlBlock : public CountControlBlock {
      private:
        Y* ptr;
        [[no_unique_address]] Deleter del;
        [[no_unique_address]] Alloc alloc;

      public:
        inline PointerControlBlock(Y* ptr, Deleter del, Alloc alloc) noexcept
            : CountControlBlock(), ptr(ptr), del(std::move(del)), alloc(std::move(alloc)) {
        }

        void destroy_object() noexcept override {
          del(ptr);
          del.~Deleter();
        }

        void destroy_self() noexcept override {
          using SelfAlloc =
              std::allocator_traits<Alloc>::template rebind_alloc<PointerControlBlock>;
          using SelfTraits = std::allocator_traits<SelfAlloc>;
          using SelfPointer = std::pointer_traits<typename SelfTraits::pointer>;

          SelfAlloc self_alloc(alloc);
          alloc.~Alloc();
          SelfTraits::deallocate(self_alloc, SelfPointer::pointer_to(*this), 1);
        }
    };

    T* ptr = nullptr;
    CountControlBlock* block = nullptr;

    template <class Alloc>
    inline SharedPtr(ObjectControlBlock<Alloc>* block) : ptr(block->ptr()), block(block) {
      ++block->count;
    }

    template <typename U>
    friend class WeakPtr;

    inline explicit SharedPtr(const WeakPtr<T>& other) : ptr(other.ptr), block(other.block) {
      ++block->count;
    }

    template <typename U, typename Alloc, typename... Args>
    friend SharedPtr<U> allocateShared(const Alloc& alloc, Args&&... args);

    template <typename U>
    friend class SharedPtr;

    inline void destroy_block() noexcept {
      block->destroy_self();
      block = nullptr;
    }

    inline void destroy_object() noexcept {
      block->destroy_object();
      ptr = nullptr;
      if (block->weak_count == 0) {
        destroy_block();
      }
    }

    inline void decrement() noexcept {
      if (block && --block->count == 0) {
        destroy_object();
      }
    }

    inline void increment() noexcept {
      if (block) {
        ++block->count;
      }
    }

  public:
    [[nodiscard]] inline size_t use_count() const noexcept {
      return block ? block->count : 0;
    }

    [[nodiscard]] inline bool expired() const noexcept {
      return use_count() == 0;
    }

    [[nodiscard]] inline T& operator*() noexcept {
      return *ptr;
    }

    [[nodiscard]] inline const T& operator*() const noexcept {
      return *ptr;
    }

    [[nodiscard]] inline T* operator->() noexcept {
      return ptr;
    }

    [[nodiscard]] inline const T* operator->() const noexcept {
      return ptr;
    }

    [[nodiscard]] inline T* get() noexcept {
      return ptr;
    }

    [[nodiscard]] inline const T* get() const noexcept {
      return ptr;
    }

    inline void reset() noexcept {
      SharedPtr().swap(*this);
    }

    template <points_as<T> Y>
    inline void reset(Y* ptr) {
      SharedPtr<T>(ptr).swap(*this);
    }

    template <points_as<T> Y, class Deleter>
    inline void reset(Y* ptr, Deleter del) {
      SharedPtr<T>(ptr, std::move(del)).swap(*this);
    }

    template <points_as<T> Y, class Deleter, class Alloc>
    inline void reset(Y* ptr, Deleter del, Alloc alloc) {
      SharedPtr<T>(ptr, std::move(del), std::move(alloc)).swap(*this);
    }

    inline SharedPtr() noexcept = default;

    template <points_as<T> Y>
    inline explicit SharedPtr(Y* ptr) : SharedPtr(ptr, std::default_delete<Y>{}) {
    }

    template <points_as<T> Y, class Deleter>
    inline SharedPtr(Y* ptr, Deleter del) : SharedPtr(ptr, std::move(del), std::allocator<Y>()) {
    }

    template <points_as<T> Y, class Deleter, class Alloc>
    inline SharedPtr(Y* ptr, Deleter del, Alloc alloc) : ptr(ptr) {
      using BlockAlloc = std::allocator_traits<Alloc>::template rebind_alloc<
          PointerControlBlock<Y, Deleter, Alloc>>;
      using BlockTraits = std::allocator_traits<BlockAlloc>;
      BlockAlloc block_alloc(alloc);
      auto allocated = BlockTraits::allocate(block_alloc, 1);
      try {
        ::new (allocated)
            PointerControlBlock<Y, Deleter, Alloc>(ptr, std::move(del), std::move(alloc));
      } catch (...) {
        BlockTraits::deallocate(block_alloc, allocated, 1);
        throw;
      }
      block = allocated;
      increment();
    }

    template <points_as<T> Y>
    inline SharedPtr(const SharedPtr<Y>& other) noexcept
        : ptr(other.ptr), block(const_cast<CountControlBlock*>(other.block)) {
      increment();
    }

    inline SharedPtr(const SharedPtr& other) noexcept
        : ptr(other.ptr), block(const_cast<CountControlBlock*>(other.block)) {
      increment();
    }

    template <points_as<T> Y>
    inline SharedPtr(const SharedPtr<Y>& other, T* ptr) noexcept : SharedPtr(other), ptr(ptr) {
    }

    template <points_as<T> Y>
    inline SharedPtr(SharedPtr<Y>&& other, T* ptr) noexcept : SharedPtr(other), ptr(ptr) {
    }

    template <points_as<T> Y>
    inline SharedPtr& operator=(const SharedPtr<Y>& other) noexcept {
      SharedPtr<T>(other).swap(*this);
      return *this;
    }

    inline SharedPtr& operator=(const SharedPtr& other) noexcept {
      if (this != std::addressof(other)) {
        SharedPtr(other).swap(*this);
      }
      return *this;
    }

    template <points_as<T> Y>
    inline SharedPtr(SharedPtr<Y>&& other) noexcept : ptr(other.ptr), block(other.block) {
      other.block = nullptr;
    }

    inline SharedPtr(SharedPtr&& other) noexcept : ptr(other.ptr), block(other.block) {
      other.block = nullptr;
    }

    template <points_as<T> Y>
    inline SharedPtr& operator=(SharedPtr<Y>&& other) noexcept {
      SharedPtr<T>(std::move(other)).swap(*this);
      return *this;
    }

    inline SharedPtr& operator=(SharedPtr&& other) noexcept {
      SharedPtr<T>(std::move(other)).swap(*this);
      return *this;
    }

    inline void swap(SharedPtr& other) noexcept {
      std::swap(ptr, other.ptr);
      std::swap(block, other.block);
    }

    inline ~SharedPtr() noexcept {
      decrement();
    }
};

template <class T, class Alloc, class... Args>
inline SharedPtr<T> allocateShared(const Alloc& alloc, Args&&... args) {
  using Block = SharedPtr<T>::template ObjectControlBlock<Alloc>;
  using BlockAlloc = std::allocator_traits<Alloc>::template rebind_alloc<Block>;
  using BlockTraits = std::allocator_traits<BlockAlloc>;

  BlockAlloc block_alloc(alloc);
  auto block = BlockTraits::allocate(block_alloc, 1);
  try {
    BlockTraits::construct(block_alloc, block, alloc, std::forward<Args>(args)...);
  } catch (...) {
    BlockTraits::deallocate(block_alloc, block, 1);
    throw;
  }
  return SharedPtr<T>(block);
}

template <class T, class... Args>
inline SharedPtr<T> makeShared(Args&&... args) {
  return allocateShared<T, std::allocator<T>, Args...>(std::allocator<T>{},
                                                       std::forward<Args>(args)...);
}

template <typename T>
class WeakPtr {
  private:
    T* ptr = nullptr;
    CountControlBlock* block = nullptr;

    template <typename U>
    friend class WeakPtr;

    template <typename U>
    friend class SharedPtr;

    inline void increment() noexcept {
      if (block) {
        ++block->weak_count;
      }
    }

  public:
    [[nodiscard]] inline size_t use_count() const noexcept {
      return (block ? block->count : 0);
    }

    [[nodiscard]] inline bool expired() const noexcept {
      return use_count() == 0;
    }

    inline WeakPtr() noexcept = default;

    template <points_as<T> Y>
    inline WeakPtr(const SharedPtr<Y>& shared) noexcept : ptr(shared.ptr), block(shared.block) {
      increment();
    }

    template <points_as<T> Y>
    inline WeakPtr(const WeakPtr<Y>& weak) noexcept : ptr(weak.ptr), block(weak.block) {
      increment();
    }

    inline WeakPtr(const WeakPtr& other) noexcept : ptr(other.ptr), block(other.block) {
      increment();
    }

    template <points_as<T> Y>
    inline WeakPtr& operator=(const WeakPtr<Y>& other) noexcept {
      WeakPtr<T>(other).swap(*this);
      return *this;
    }

    inline WeakPtr& operator=(const WeakPtr& other) noexcept {
      if (this != std::addressof(other)) {
        WeakPtr(other).swap(*this);
      }
      return *this;
    }

    template <points_as<T> Y>
    inline WeakPtr(WeakPtr<Y>&& other) noexcept : ptr(other.ptr), block(other.block) {
      other.block = nullptr;
    }

    inline WeakPtr(WeakPtr&& other) noexcept : ptr(other.ptr), block(other.block) {
      other.block = nullptr;
    }

    template <points_as<T> Y>
    inline WeakPtr& operator=(WeakPtr<Y>&& other) noexcept {
      WeakPtr<T>(std::move(other)).swap(*this);
      return *this;
    }

    inline WeakPtr& operator=(WeakPtr&& other) noexcept {
      WeakPtr<T>(std::move(other)).swap(*this);
      return *this;
    }

    inline void swap(WeakPtr& other) noexcept {
      std::swap(ptr, other.ptr);
      std::swap(block, other.block);
    }

    inline SharedPtr<T> lock() const noexcept {
      return (expired() ? SharedPtr<T>() : SharedPtr<T>(*this));
    }

    inline ~WeakPtr() noexcept {
      if (block && --block->weak_count == 0 && block->count == 0) {
        block->destroy_self();
        block = nullptr;
      }
    }
};

template <class T>
SharedPtr(WeakPtr<T>) -> SharedPtr<T>;

#endif
