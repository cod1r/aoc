#include "../aoc_utils.h"
#include <cstdint>
#include <stdexcept>
#include <string>
#include <cassert>
#include <climits>
#include <map>

struct DirectionalKeyPad { 
  char current_state = 'A';
};

struct NumericKeyPad {
  char current_state = 'A';
};

const std::vector<std::string> dir_pad = {
  " ^A",
  "<v>",
};
const std::vector<std::string> num_pad = {
  "789",
  "456",
  "123",
  " 0A"
};

std::vector<std::string> dfsPath(std::pair<int, int> s, std::pair<int, int> e, std::string path, const std::vector<std::string>& pad) {
  if (s == e) return {path + 'A'};
  std::vector<std::string> paths;
  if (pad[s.second][s.first] == ' ') return paths;
  if (s.first < e.first) {
    auto colUp = dfsPath({s.first + 1, s.second}, e, path + '>', pad);
    paths.insert(paths.end(), colUp.begin(), colUp.end());
  } else if (s.first > e.first) {
    auto colDown = dfsPath({ s.first - 1, s.second }, e, path + '<', pad);
    paths.insert(paths.end(), colDown.begin(), colDown.end());
  }

  if (s.second < e.second) {
    auto colUp = dfsPath({s.first, s.second + 1 }, e, path + 'v', pad);
    paths.insert(paths.end(), colUp.begin(), colUp.end());
  } else if (s.second > e.second) {
    auto colDown = dfsPath({s.first, s.second - 1 }, e, path + '^', pad);
    paths.insert(paths.end(), colDown.begin(), colDown.end());
  }

  return paths;
}

std::vector<std::string> pathFind(char start, char end, const std::vector<std::string>& pad) {
  std::pair<int, int> start_point;
  std::pair<int, int> end_point;
  std::pair<int, int> blank_point;
  for (int r = 0; r < pad.size(); ++r) {
    for (int c = 0; c < pad[r].size(); ++c) {
      if (pad[r][c] == ' ') {
        blank_point = { c, r };
      }
      if (pad[r][c] == start) {
        start_point = { c, r };
      }
      if (pad[r][c] == end) {
        end_point = { c , r };
      }
    }
  }
  auto moveColumn = [&start_point, &end_point, &pad](std::string& instructions) -> void {
    while (start_point.first != end_point.first) {
      if (start_point.first - 1 >= 0 &&
        pad[start_point.second][start_point.first - 1] == ' ' && start_point.first > end_point.first) {
        break;
      }
      if (start_point.first + 1 < pad[start_point.second].size() &&
        pad[start_point.second][start_point.first + 1] == ' ' && start_point.first < end_point.first) {
        break;
      }
      if (start_point.first < end_point.first) {
        instructions.append(">");
        start_point.first++;
      } else {
        instructions.append("<");
        start_point.first--;
      }
    }
  };
  auto moveRow = [&start_point, &end_point, &pad](std::string& instructions) -> void {
    while (start_point.second != end_point.second) {
      if (start_point.second - 1 >= 0 &&
        pad[start_point.second - 1][start_point.first] == ' ' && start_point.second > end_point.second) {
        break;
      }
      if (start_point.second + 1 < pad.size() &&
        pad[start_point.second + 1][start_point.first] == ' ' && start_point.second < end_point.second) {
        break;
      }
      if (start_point.second < end_point.second) {
        instructions.push_back('v');
        start_point.second++;
      } else {
        instructions.push_back('^');
        start_point.second--;
      }
    }
  };

  std::vector<std::string> multiple_ways = dfsPath(start_point, end_point, "", pad);
  /*if (start_point.first == blank_point.first && start_point.first != end_point.first) {*/
  /*  std::string instructions;*/
  /*  moveColumn(instructions);*/
  /*  moveRow(instructions);*/
  /*  instructions.push_back('A');*/
  /*  multiple_ways.push_back(instructions);*/
  /*assert(start_point == end_point);*/
  /*} else if (start_point.second == blank_point.second && start_point.second != end_point.second) {*/
  /*  std::string instructions;*/
  /*  moveRow(instructions);*/
  /*  moveColumn(instructions);*/
  /*  instructions.push_back('A');*/
  /*  multiple_ways.push_back(instructions);*/
  /*assert(start_point == end_point);*/
  /*} else {*/
  /*  auto start_point_copy = start_point;*/
  /*  {*/
  /*    std::string instructions_row_first;*/
  /*    moveRow(instructions_row_first);*/
  /*    moveColumn(instructions_row_first);*/
  /*    instructions_row_first.push_back('A');*/
  /*    multiple_ways.push_back(instructions_row_first);*/
  /*assert(start_point == end_point);*/
  /*  }*/
  /*  {*/
  /*    start_point = start_point_copy;*/
  /*    std::string instructions_col_first;*/
  /*    moveColumn(instructions_col_first);*/
  /*    moveRow(instructions_col_first);*/
  /*    instructions_col_first.push_back('A');*/
  /*    multiple_ways.push_back(instructions_col_first);*/
  /*  }*/
  /*}*/

  return multiple_ways;
}

struct Keypads {
  std::vector<DirectionalKeyPad> dirpads = std::vector(26, DirectionalKeyPad{});
};

#define MEMO 1

uint64_t buildString(const std::string& path, int level, Keypads& kpds, int limit, std::map<std::tuple<std::string, int>, uint64_t>& memo) {
  if (level == limit) return path.size();
#if MEMO
  if (memo.count({ path, level })) return memo[{ path, level }];
#endif
  uint64_t accum = 0;
  char initialState = kpds.dirpads[level].current_state;
  for (int i = 0; i < path.size(); ++i) {
    char start = i == 0 ? initialState : path[i - 1];
    char end = path[i];
    std::vector<std::string> newPaths = pathFind(start, end, level == 0 ? num_pad : dir_pad);
    uint64_t shortest = ULLONG_MAX;
    std::string chosenPath;
    for (const auto& innerpath : newPaths) {
      if (innerpath.empty()) continue;
      uint64_t built = buildString(innerpath, level + 1, kpds, limit, memo);
      assertWithMessage(innerpath.back() != 0, std::to_string(innerpath.size()));
      kpds.dirpads[level].current_state = innerpath.back();
      if (built < shortest) {
        shortest = built;
        chosenPath = innerpath;
      }
    }
    assertWithMessage(newPaths.size() > 0, "\"" + std::to_string(start) + ":" + std::to_string(end) + ":" + path + ":" + std::to_string(i) + ":" + std::to_string(initialState) + "\"");
    assertWithMessage(shortest != ULLONG_MAX, std::string(1, start) + ":" + end);
    accum += shortest;
  }
#if MEMO
  memo[{ path, level }] = accum;
#endif
  return accum;
}


// the issue is the keypad states. each keypad needs to finish/start with the right state depending on the output from newPaths (which path has the shortest path)

int main() {
  std::string contents = readFile("day21.input");
  std::vector<std::string> lines = split(contents, "\n");
  uint64_t ans = 0;
  for (const auto& code : lines) {
    Keypads kpds;
    std::map<std::tuple<std::string, int>, uint64_t> memo;
    uint64_t built = buildString(code, 0, kpds, 3, memo);
    ans += built * std::stoi(code.substr(0, code.size() - 1));
  }
  std::cout << ans << std::endl;
  //219314 too high
  // A -> 0 : <A
  // < -> A : <V<A | v<<A ;; 2nd dir pad -> first dir pad has to get from A to < so the second has to do <v<A or v<<A
  // -
  // <v<A>A<A
  // <vA<AA


  //8720022168 too low
  //13196733644 too low
  //26603027419036 too low
  //60749353942522 not right
  //83265069542382828 not right
  //153692276965332 not right
  //388832376934298 not right
  //1049204621038404 not right
  //414715070779322 not right
  //260586897262600 RIGHT!!!!

  uint64_t ans2 = 0;
  for (const auto& code : lines) {
    Keypads kpds;
    std::map<std::tuple<std::string, int>, uint64_t> memo;
    uint64_t built = buildString(code, 0, kpds, 26, memo);
    ans2 += built * std::stoi(code.substr(0, code.size() - 1));
  }
  std::cout << ans2 << std::endl;
  return 0;
}
