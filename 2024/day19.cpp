#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <fstream>
#include <stdexcept>
#include <string_view>
#include <vector>
#include <string>
#include <set>
#include <map>
#include <queue>

std::vector<std::string> split(std::string s, std::string splitOn) {
 std::vector<std::string> strings;
 size_t start = 0;
 size_t foundIdx = 0;
 while ((foundIdx = s.find(splitOn, start)) != -1) {
  const auto& subs = s.substr(start, foundIdx - start);
  strings.push_back(subs);
  start = foundIdx + splitOn.length();
 }
 return strings;
}

bool dfs(const std::vector<std::string>& towels, const std::string_view& pattern, std::map<std::string_view, bool>& memo) {
  if (memo.count(pattern)) return memo[pattern];
  if (pattern == "") {
    return true;
  }
  for (const std::string& t : towels) {
    if (pattern.find(t) == 0) {
      const auto& valid = dfs(
        towels,
        pattern.substr(t.length()),
        memo);
      if (valid) {
        memo.insert({pattern, valid});
        return true;
      }
    }
  }
  memo.insert({pattern, false});
  return false;
}

int main(void) {
  std::ifstream input("day19.input");
  if (!input.is_open()) {
    throw std::runtime_error("input file cannot be opened");
  }
  std::string line;
  std::vector<std::string> towels;
  std::vector<std::string> patterns;
  while (!input.eof()) {
    std::getline(input, line);
    if (line.length() > 0 && line.find(",") != -1) {
      const auto& splitOnCommas = split(line, ", ");
      towels.insert(towels.end(), splitOnCommas.begin(), splitOnCommas.end());
    } else if (line.length() > 0) {
      patterns.push_back(line);
    }
  }
  int ans = 0;
  for (const auto& p : patterns) {
    std::map<std::string_view, bool> memo;
    /*std::cout << "solving " << p << std::endl;*/
    const auto& found = dfs(towels, p, memo);
    if (found) {
      ++ans;
    }
  }

  std::cout << ans << std::endl;
  return 0;
}
