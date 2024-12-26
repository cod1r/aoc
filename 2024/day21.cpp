#include "../aoc_utils.h"
#include <cstddef>
#include <string>
#include <tuple>
#include <algorithm>
#include <map>
#include <cassert>
#include <climits>

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
        instructions.append("v");
        start_point.second++;
      } else {
        instructions.append("^");
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
    instructions.append("A");
    multiple_ways.push_back(instructions);
  } else if (start_point.second == blank_point.second && start_point.second != end_point.second) {
    std::string instructions;
    moveRow(instructions);
    moveColumn(instructions);
    moveRow(instructions);
    instructions.append("A");
    multiple_ways.push_back(instructions);
  } else {
    auto start_point_copy = start_point;
    {
      std::string instructions_row_first;
      moveRow(instructions_row_first);
      moveColumn(instructions_row_first);
      instructions_row_first.append("A");
      multiple_ways.push_back(instructions_row_first);
    }
    {
      start_point = start_point_copy;
      std::string instructions_col_first;
      moveColumn(instructions_col_first);
      moveRow(instructions_col_first);
      instructions_col_first.append("A");
      multiple_ways.push_back(instructions_col_first);
    }
  }

  assert(start_point == end_point);
  return multiple_ways;
}

char root_resolver(char current_state, char direction, const std::vector<std::string>& pad) {
  for (int row = 0; row < pad.size(); ++row) {
    for (int idx = 0; idx < pad[row].size(); ++idx) {
      if (dir_pad[row][idx] == current_state) {
        switch (direction) {
          case '^': {
            if (row - 1 >= 0) return dir_pad[row - 1][idx];
          } break;
          case '>': {
            if (idx + 1 < dir_pad[row].size()) return dir_pad[row][idx + 1];
          } break;
          case 'v': {
            if (row + 1 < 2) return dir_pad[row + 1][idx];
          } break;
          case '<': {
            if (idx - 1 >= 0) return dir_pad[row][idx - 1];
          } break;
        }
      }
    }
  }
  return current_state;
}

char directional_resolver(const DirectionalKeyPad& kp, char direction) {
  return root_resolver(kp.current_state, direction, dir_pad);
}
char numeric_resolver(const NumericKeyPad& np, char direction) {
  return root_resolver(np.current_state, direction, num_pad);
}

struct Keypads {
  NumericKeyPad numbers{};
  DirectionalKeyPad dir_pad1{};
  DirectionalKeyPad dir_pad2{};
};

std::string buildString(const std::string& path, int level, Keypads& kpds) {
  if (level == 3) return path;
  std::string accum;
  char initialState;
  switch (level) {
    case 0: initialState = kpds.numbers.current_state; break;
    case 1: initialState = kpds.dir_pad1.current_state; break;
    case 2: initialState = kpds.dir_pad2.current_state; break;
  }
  for (int i = 0; i < path.size(); ++i) {
    std::vector<std::string> newPaths = pathFind(i == 0 ? initialState : path[i - 1], path[i], level == 0 ? num_pad : dir_pad);
    std::string shortest;
    for (const auto& path : newPaths) {
      std::string built = buildString(path, level + 1, kpds);
      if (shortest.size() == 0 || shortest.size() > built.size()) {
        shortest = built;
      }
    }
    accum.append(shortest);
  }
  switch (level) {
    case 0: kpds.numbers.current_state = path.back(); break;
    case 1: kpds.dir_pad1.current_state = path.back(); break;
    case 2: kpds.dir_pad2.current_state = path.back(); break;
  }
  return accum;
}

int findMinLength(const std::vector<std::vector<std::string>>& parts, int i, std::string acc, std::map<int, int>& memo) {
  if (memo.count(i)) return memo[i];
  if (i == parts.size()) return acc.size();
  size_t least = INT_MAX;
  for (const std::string& s : parts[i]) {
    int result = findMinLength(parts, i + 1, s, memo);
    least = std::min(least, acc.size() + result);
  }
  memo[i] = least;
  return least;
}

int main() {
  std::string contents = readFile("day21.input");
  std::vector<std::string> lines = split(contents, "\n");
  uint64_t ans = 0;
  for (const auto& code : lines) {
    Keypads kpds;
    std::string built = buildString(code, 0, kpds);
    ans += built.size() * std::stoi(code.substr(0, code.size() - 1));
  }
  std::cout << ans << std::endl;
  //219314 too high
  // A -> 0 : <A
  // < -> A : <V<A | v<<A ;; 2nd dir pad -> first dir pad has to get from A to < so the second has to do <v<A or v<<A
  // -
  // <v<A>A<A
  // <vA<AA
  return 0;
}
