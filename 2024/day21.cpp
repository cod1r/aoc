#include "../aoc_utils.h"
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

std::vector<std::string> pathFind(char start, char end, std::vector<std::string> pad) {
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

  std::vector<std::string> multiple_ways;
  if (start_point.first == blank_point.first && start_point.first != end_point.first) {
    std::string instructions;
    moveColumn(instructions);
    moveRow(instructions);
    moveColumn(instructions);
    instructions.push_back('A');
    multiple_ways.push_back(instructions);
  } else if (start_point.second == blank_point.second && start_point.second != end_point.second) {
    std::string instructions;
    moveRow(instructions);
    moveColumn(instructions);
    moveRow(instructions);
    instructions.push_back('A');
    multiple_ways.push_back(instructions);
  } else {
    auto start_point_copy = start_point;
    {
      std::string instructions_row_first;
      moveRow(instructions_row_first);
      moveColumn(instructions_row_first);
      instructions_row_first.push_back('A');
      multiple_ways.push_back(instructions_row_first);
    }
    {
      start_point = start_point_copy;
      std::string instructions_col_first;
      moveColumn(instructions_col_first);
      moveRow(instructions_col_first);
      instructions_col_first.push_back('A');
      multiple_ways.push_back(instructions_col_first);
    }
  }

  assert(start_point == end_point);
  return multiple_ways;
}

struct Keypads {
  NumericKeyPad numbers{};
  std::vector<DirectionalKeyPad> dirpads = std::vector(25, DirectionalKeyPad{});
};

uint64_t buildString(const std::string& path, int level, Keypads& kpds, int limit, std::map<std::tuple<char, char, int>, uint64_t>& memo) {
  if (level == limit) return path.size();
  uint64_t accum = 0;
  char initialState;
  switch (level) {
    case 0: initialState = kpds.numbers.current_state; break;
    default: initialState = kpds.dirpads[level].current_state; break;
  }
  for (int i = 0; i < path.size(); ++i) {
    char start = i == 0 ? initialState : path[i - 1];
    char end = path[i];
    std::vector<std::string> newPaths = pathFind(start, end, level == 0 ? num_pad : dir_pad);
    uint64_t shortest = ULLONG_MAX;
    if (memo.count({ start, end, level })) {
      accum += memo[{ start, end, level }];
      continue;
    }
    for (const auto& innerpath : newPaths) {
      uint64_t built = buildString(innerpath, level + 1, kpds, limit, memo);
      shortest = std::min(shortest, built);
    }
    memo[{ start, end, level }] = shortest;
    accum += shortest;
  }
  switch (level) {
    case 0: kpds.numbers.current_state = path.back(); break;
    default: kpds.dirpads[level].current_state = path.back(); break;
  }
  return accum;
}

int main() {
  std::string contents = readFile("day21.input");
  std::vector<std::string> lines = split(contents, "\n");
  std::map<std::tuple<char, char, int>, uint64_t> memo;
  /*constexpr std::string_view dirpadchars = "<>^vA";*/
  /*for (int i = 0; i < dirpadchars.size(); ++i) {*/
  /*  for (int j = i; j < dirpadchars.size(); ++j) {*/
  /*    for (int l = 1; l < 25; ++l) {*/
  /*      std::vector<std::string> newPaths = pathFind(dirpadchars[i], dirpadchars[j], dir_pad);*/
  /*      Keypads kpds;*/
  /*      for (const auto& p : newPaths) {*/
  /*        uint64_t built = buildString(p, l, kpds, 24, memo);*/
  /*      }*/
  /*      memo[{ dirpadchars[i], dirpadchars[j], l }] = shortest;*/
  /*    }*/
  /*  }*/
  /*}*/
  uint64_t ans = 0;
  for (const auto& code : lines) {
    Keypads kpds;
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
  std::map<std::tuple<char, char, int>, uint64_t> memo2;
  uint64_t ans2 = 0;
  for (const auto& code : lines) {
    Keypads kpds;
    uint64_t built = buildString(code, 0, kpds, 10, memo2);
    ans2 += built * std::stoi(code.substr(0, code.size() - 1));
  }
  std::cout << ans2 << std::endl;
  //8720022168 too low
  //13196733644 too low
  //26603027419036 too low
  //60749353942522 not right
  //83265069542382828 not right
  return 0;
}
