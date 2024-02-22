#include <iostream>

template <typename T>
void create_stack(T*& stack, size_t capacity) {
  stack = new T[capacity];
}

template <typename T>
void reallocate_stack(T*& stack, size_t& size, size_t& capacity, size_t new_capacity) {
  T* new_stack = new T[new_capacity];
  if (new_capacity < size) {
    size = new_capacity;
  }
  for (size_t i = 0; i < size; ++i) {
    new_stack[i] = stack[i];
  }
  delete[] stack;
  capacity = new_capacity;
  stack = new_stack;
}

template <typename T>
void push_to_stack(T*& stack, size_t& size, size_t& capacity, const T& x) {
  if (size + 1 > capacity) {
    reallocate_stack(stack, size, capacity, capacity * 2);
  }
  stack[size++] = x;
}

template <typename T>
T& stack_top(T*& stack, size_t size) {
  return stack[size - 1];
}

template <typename T>
void pop_from_stack(T*& stack [[maybe_unused]], size_t& size) {
  --size;
}

void read_string(char*& string, size_t& size, size_t& capacity) {
  const size_t BUFFER_SIZE = 256;
  char buffer[BUFFER_SIZE];
  capacity = 1;
  size = 0;
  create_stack(string, capacity);
  bool is_line_end_reached = false;
  while (!is_line_end_reached) {
    std::cin.getline(buffer, BUFFER_SIZE);
    for (char i : buffer) {
      push_to_stack(string, size, capacity, i);
      if (i == '\0') {
        is_line_end_reached = true;
        break;
      }
    }
  }
}

std::ostream& write_string(std::ostream& out, const char* string) {
  for (size_t i = 0; string[i] != '\0'; ++i) {
    out.put(string[i]);
  }
  return out;
}

enum Command { PUSH = 'u', POP = 'o', BACK = 'a', SIZE = 'i', CLEAR = 'l', EXIT = 'x' };

Command read_command() {
  char trash = 0;
  char command = 0;
  std::cin.get(trash);
  std::cin.get(command);
  std::cin.get(trash);
  while (trash != ' ' && trash != '\n') {
    std::cin.get(trash);
  }
  return static_cast<Command>(command);
}

template <typename T>
void pop_from_on_heap_stack(T*& stack, size_t& size) {
  if (size == 0) {
    return;
  }
  delete[] stack_top(stack, size);
  pop_from_stack(stack, size);
}

template <typename T>
void clear_on_heap_stack(T*& stack, size_t& size) {
  while (size > 0) {
    pop_from_on_heap_stack(stack, size);
  }
}

enum Message { OK, ERROR, BYE };

template <bool cond_v, typename Then, typename OrElse>
decltype(auto) constexpr_if(Then&& then, OrElse&& or_else) {
  if constexpr (cond_v) {
    return static_cast<Then&&>(then);
  } else {
    return static_cast<OrElse&&>(or_else);
  }
}

template <Message message>
void write_message() {
  for (char c : constexpr_if<message == OK>("ok", constexpr_if<message == ERROR>("error", "bye"))) {
    if (c) {
      std::cout.put(c);
    }
  }
}

int main() {
  char** stringStack = nullptr;
  size_t size = 0;
  size_t capacity = 1;
  create_stack(stringStack, capacity);

  bool reached_termination = false;
  while (!reached_termination) {
    Command command = read_command();
    if (command == PUSH) {
      char* string = nullptr;
      size_t string_size = 0;
      size_t string_capacity = 0;
      read_string(string, string_size, string_capacity);
      push_to_stack(stringStack, size, capacity, string);
      write_message<OK>();
    } else if (command == BACK || command == POP) {
      if (size == 0) {
        write_message<ERROR>();
      } else {
        char* top = stack_top(stringStack, size);
        write_string(std::cout, top);
        if (command == POP) {
          pop_from_on_heap_stack(stringStack, size);
        }
      }
    } else if (command == SIZE) {
      std::cout << size;
    } else if (command == CLEAR) {
      clear_on_heap_stack(stringStack, size);
      write_message<OK>();
    } else if (command == EXIT) {
      write_message<BYE>();
      reached_termination = true;
    }
    std::cout.put('\n');
  }

  clear_on_heap_stack(stringStack, size);

  delete[] stringStack;
}
