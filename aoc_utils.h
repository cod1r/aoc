#define AOC_UTILS
#ifdef AOC_UTILS
#include <vector>
#include <string>
#include <fstream>
#include <iostream>

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
    if (line.length() > 0) {
      contents += line + '\n';
    }
  }
  return contents;
}

#endif
