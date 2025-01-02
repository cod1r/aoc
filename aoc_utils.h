#define AOC_UTILS
#ifdef AOC_UTILS
#include <functional>
#include <vector>
#include <string>
#include <fstream>
#include <iostream>

template<typename T>
std::vector<T> filter(const std::vector<T>& v, std::function<bool(const T&)> lambda) {
  std::vector<T> filtered;
  for (const auto& e : v) {
    if (lambda(e)) {
      filtered.push_back(e);
    }
  }
  return filtered;
}

template<typename T>
bool includes(const std::vector<T>& v, const T& e) {
  for (const T& thing : v) if (thing == e) return true;
  return false;
}

std::string join(std::vector<std::string> v, std::string joinSeq) {
  std::string res;
  for (int i = 0; i < v.size(); ++i) {
    const auto& s = v[i];
    res.append(s + (i == v.size() - 1 ? "" : joinSeq));
  }
  return res;
}

template<typename T>
std::vector<std::vector<T>> subsets(const std::vector<T>& set, int index, std::vector<T> newSet) {
  if (index == set.size()) return {newSet};
  std::vector<std::vector<T>> all;
  const auto& e = set[index];
  auto notTaken = subsets(set, index + 1, newSet);
  all.insert(all.end(), notTaken.begin(), notTaken.end());
  newSet.push_back(e);
  auto taken = subsets(set, index + 1, newSet);
  newSet.pop_back();
  all.insert(all.end(), taken.begin(), taken.end());
  return all;
}

std::vector<std::pair<int, int>> generatePairsIndices(int amount) {
  std::vector<std::pair<int, int>> pairs;
  for (int i = 0; i < amount; ++i) {
    for (int j = i + 1; j < amount; ++j) {
      pairs.push_back({i, j});
    }
  }
  return pairs;
}

std::vector<std::string> split(std::string s, std::string splitOn) {
 std::vector<std::string> strings;
 size_t start = 0;
 size_t foundIdx = 0;
 while ((foundIdx = s.find(splitOn, start)) != -1) {
  const auto& subs = s.substr(start, foundIdx - start);
  strings.push_back(subs);
  start = foundIdx + splitOn.length();
 }
 if (start < s.length()) {
   strings.push_back(s.substr(start, s.length() - start));
 }
 return strings;
}

std::ostream& operator<<(std::ostream& os, const std::pair<int, int> p) {
  return os << p.first << " " << p.second;
}

template<typename t>
std::ostream& operator<<(std::ostream& os, const std::vector<t>& v) {
  for (const auto& e : v) {
    os << e << " ";
  }
  return os;
}

void assertWithMessage(bool condition, std::string msg) {
  if (!condition) {
    throw std::runtime_error(msg);
  }
}

std::string readFile(std::string filePath) {
  std::ifstream input(filePath);
  if (!input.is_open()) {
    throw std::runtime_error("input file cannot be opened");
  }
  std::string contents;
  std::string line;
  while (!input.eof()) {
    std::getline(input, line);
    contents += line + '\n';
  }
  return contents;
}

#endif
