#include "../aoc_utils.h"
#include <bitset>
#include <set>
#include <map>
#include <algorithm>
#include <cassert>

int AND(int a, int b) {
  assert(a <= 1);
  assert(b <= 1);
  return a & b;
}
int XOR(int a, int b) {
  assert(a <= 1);
  assert(b <= 1);
  return a ^ b;
}
int OR(int a, int b) {
  assert(a <= 1);
  assert(b <= 1);
  return a | b;
}

int main(void) {
  std::string contents = readFile("day24.input");
  std::vector<std::string> lines = split(contents, "\n");
  if (lines.back() == "") lines.pop_back();
  std::map<std::string, int64_t> values;
  for (const auto& s : lines) {
    if (s == "") break;
    auto idxColon = s.find(":");
    if (idxColon != std::string::npos) {
      std::string wire = s.substr(0, idxColon);
      std::string value = s.substr(idxColon + 2, s.size() - (idxColon + 2));
      values.insert({ wire, std::stoi(value) });
    }
  }
  int start = -1;
  for (int i = 0; i < lines.size(); ++i) {
    if (lines[i] == "") {
      start = i + 1;
      break;
    }
  }
  assert(start != -1);
  std::vector<std::tuple<std::tuple<std::string, std::string, std::string_view>, std::string>> graph;
  for (;start < lines.size(); ++start) {
    const auto& s = lines[start];
    std::vector<std::string> splitted = split(s, " ");
    auto getOp = [&](const std::string_view& op) {
      if (op == "AND") return "AND";
      if (op == "XOR") return "XOR";
      return "OR";
    };
    graph.push_back({ { splitted[0], splitted[2], getOp(splitted[1]) }, splitted[4] });
  }
  auto loop = [&]() {
    bool again = false;
    for (const auto& [i, o] : graph) {
      const auto& [w1, w2, op] = i;
      if (values.count(w1) == 0 || values.count(w2) == 0) {
        assert(values.count(o) == 0);
        again = true;
        continue;
      }
      if (values.count(o)) {
        continue;
      }

      if (op == "AND") values.insert({ o, AND(values[w1], values[w2]) });
      if (op == "XOR") values.insert({ o, XOR(values[w1], values[w2]) });
      if (op == "OR") values.insert({ o, OR(values[w1], values[w2]) });
    }
    return again;
  };
  while (loop());

  for (const auto& [i, o] : graph) {
    assert(values.count(o) > 0);
  }
  for (const auto& [i, o] : values) {
    assert(o <= 1);
  }

  std::vector<std::string> z;
  for (const auto& [w, _] : values) {
    if (w[0] == 'z') z.push_back(w);
  }
  auto cmp = [](const auto& a, const auto& b) {
    const int an = std::stoi(a.substr(1));
    const int bn = std::stoi(b.substr(1));
    return an < bn;
  };
  std::sort(z.begin(), z.end(), cmp);
  int64_t ans = 0;
  int bit = 0;
  for (const auto& s : z) {
    ans |= (values[s] << bit);
    ++bit;
  }
  std::cout << ans << std::endl;
  //1292512718 too low
  return 0;
}
