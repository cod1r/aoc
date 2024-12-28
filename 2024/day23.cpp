#include "../aoc_utils.h"
#include <algorithm>
#include <map>
#include <set>
#include <string_view>

bool allThree(
  const std::string& a,
  const std::string& b,
  const std::string& c,
  const std::map<std::string, std::vector<std::string>>& connections) {
  auto found = std::find(
  connections.at(b).begin(),
  connections.at(b).end(), a);
  auto found2 = std::find(
  connections.at(a).begin(),
  connections.at(a).end(), c);
  auto found3 = std::find(connections.at(b).begin(),
  connections.at(b).end(), c);
  if (found != connections.at(b).end() && found2 != connections.at(a).end() && found3 != connections.at(b).end()) {
    return true;
  }
  return false;
}

int main(void) {
  std::string contents = readFile("day23.input");
  std::vector<std::string> lines = split(contents, "\n");
  std::map<std::string, std::vector<std::string>> connections;
  for (const auto& s : lines) {
    const auto& splitted = split(s, "-");
    if (connections.count(splitted[0]) == 0) {
      connections.insert({ splitted[0], { splitted[1] }});
    } else {
      connections[splitted[0]].push_back(splitted[1]);
    }
    if (connections.count(splitted[1]) == 0) {
      connections.insert({ splitted[1], { splitted[0] }});
    } else {
      connections[splitted[1]].push_back(splitted[0]);
    }
  }
  int ans = 0;
  std::set<std::tuple<std::string_view, std::string_view, std::string_view>> groups;
  for (const auto& [s, v] : connections) {
    for (const auto& s1 : v) {
      for (const auto& s2 : connections[s1]) {
        std::vector<std::string_view> temp{ s, s1, s2 };
        std::sort(temp.begin(), temp.end());
        const auto& key = std::make_tuple(temp[0], temp[1], temp[2]);
        if (allThree(s, s1, s2, connections) && groups.count(key) == 0) {
          groups.insert(key);
          if (s[0] == 't' || s1[0] == 't' || s2[0] == 't') {
            ++ans;
          }
        }
      }
    }
  }
  std::cout << ans << std::endl;
  //2195 too high p1
  return 0;
}
