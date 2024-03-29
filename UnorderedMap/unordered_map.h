#include <unordered_map>

template <typename Key,
          typename T,
          typename Hash = std::hash<Key>,
          typename Equal = std::equal_to<Key>,
          typename Alloc = std::allocator<std::pair<const Key, T>>>
using UnorderedMap = std::unordered_map<Key, T, Hash, Equal, Alloc>;