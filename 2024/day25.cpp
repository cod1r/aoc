#include "../aoc_utils.h"

struct Schematic {
  std::string type;
  std::vector<int> heights;
};

int main(void) {
  std::string contents = readFile("day25.input");
  std::vector<std::string> lines = split(contents, "\n");
  std::vector<Schematic> schematics;
  std::vector<std::vector<std::string>> splitChunks;
  std::vector<std::string> chunk;
  for (int i = 0; i < lines.size(); ++i) {
    const auto& s = lines[i];
    if (s == "" || i == lines.size() - 1) {
      splitChunks.push_back(chunk);
      chunk.clear();
      continue;
    }
    chunk.push_back(s);
  }
  for (const auto& chunk : splitChunks) {
    auto found = chunk[0].find('.');
    Schematic schem;
    if (found == std::string::npos) {
      schem.type = "LOCK";
      for (int c = 0; c < chunk[0].size(); ++c) {
        int r = 1;
        while (r < chunk.size() && chunk[r][c] != '.') {
          ++r;
        }
        schem.heights.push_back(r - 1);
      }
    } else {
      schem.type = "KEY";
      for (int c = 0; c < chunk[0].size(); ++c) {
        int r = 1;
        while (r < chunk.size() && chunk[chunk.size() - 1 - r][c] != '.') {
          ++r;
        }
        schem.heights.push_back(r - 1);
      }
    }
    schematics.push_back(schem);
  }
  int ans = 0;
  for (int i = 0; i < schematics.size(); ++i) {
    for (int j = i + 1; j < schematics.size(); ++j) {
      if (schematics[i].type != schematics[j].type) {
        bool fits = true;
        for (int k = 0; k < schematics[i].heights.size(); ++k) {
          if (schematics[i].heights[k] + schematics[j].heights[k] > 5) {
            fits = false;
          }
        }
        if (fits) {
          ++ans;
        }
      }
    }
  }
  std::cout << ans << std::endl;
  return 0;
}
