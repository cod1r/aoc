#include "../aoc_utils.h"
#include <string>

int main(void) {
  std::string contents = readFile("day22.input");
  std::vector<std::string> lines = split(contents, "\n");
  std::vector<uint64_t> numbers;
  for (const auto& s : lines) {
    numbers.push_back(std::stoull(s));
  }
  for (int t = 0; t < 2000; ++t) {
    for (uint64_t& n : numbers) {
      uint64_t n_64 = n * 64;
      n = n_64 ^ n;
      n = n % 16777216;

      uint64_t n_32 = n / 32;
      n = n_32 ^ n;
      n = n % 16777216;

      uint64_t n_2048 = n * 2048;
      n = n_2048 ^ n;
      n = n % 16777216;
    }
  }
  uint64_t sum = 0;
  for (const auto& n : numbers) {
    sum += n;
  }
  std::cout << sum << std::endl;
}
