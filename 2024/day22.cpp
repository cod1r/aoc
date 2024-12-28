#include "../aoc_utils.h"
#include <string>
#include <array>
#include <map>
#include <set>
#include <cassert>

int main(void) {
  std::string contents = readFile("day22.input");
  std::vector<std::string> lines = split(contents, "\n");
  std::vector<uint64_t> numbers;
  for (const auto& s : lines) {
    numbers.push_back(std::stoull(s));
  }
  std::vector<std::vector<int>> sequences(numbers.size(), std::vector<int>{});
  for (int t = 0; t < 2000; ++t) {
    for (int i = 0; i < numbers.size(); ++i) {
      if (sequences[i].size() == 0) sequences[i].push_back(numbers[i] % 10ULL);
      uint64_t n = numbers[i];
      uint64_t n_64 = n * 64ULL;
      n = n_64 ^ n;
      n = n % 16777216ULL;

      uint64_t n_32 = n / 32ULL;
      n = n_32 ^ n;
      n = n % 16777216ULL;

      uint64_t n_2048 = n * 2048ULL;
      n = n_2048 ^ n;
      n = n % 16777216ULL;

      numbers[i] = n;
      sequences[i].push_back(numbers[i] % 10ULL);
    }
  }
  uint64_t sum = 0;
  for (const auto& n : numbers) {
    sum += n;
  }
  std::cout << sum << std::endl;
  assert(sequences[0].size() == 2001);
  std::vector<std::vector<int>> changes(numbers.size(), std::vector<int>{});
  for (int i = 0; i < sequences.size(); ++i) {
    for (int j = 1; j < sequences[i].size(); ++j) {
      changes[i].push_back(sequences[i][j] - sequences[i][j - 1]);
    }
  }
  std::map<std::tuple<int, int, int, int>, int> fours;
  for (int ii = 0; ii < changes.size(); ++ii) {
    std::set<std::tuple<int, int, int, int>> added;
    for (int idxs = 3; idxs < changes[ii].size(); ++idxs) {
      std::tuple<int, int, int, int> key = {
        changes[ii][idxs - 3],
        changes[ii][idxs - 2],
        changes[ii][idxs - 1],
        changes[ii][idxs]
      };
      if (fours.count(key) == 0) {
        fours.insert({key, sequences[ii][idxs + 1]});
        added.insert(key);
      } else if (added.count(key) == 0) {
        added.insert(key);
        fours[key] += sequences[ii][idxs + 1];
      }
    }
  }
  uint64_t ans2 = 0;
  for (const auto& [key, value] : fours) {
    const auto& [d1, d2, d3, d4] = key;
    ans2 = std::max(ans2, (uint64_t)value);
  }
  std::cout << ans2 << std::endl;
  //3609 too high
  //2066 too low
  //2304 too high
  //2295 not right
  //2121 RIGHT
  return 0;
}
