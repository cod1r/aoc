#include "../aoc_utils.h"
#include <algorithm>
#include <climits>
#include <set>
#include <queue>
#include <iostream>

struct Node {
  int x;
  int y;
  int steps;
};

const int UP = 1;
const int DOWN = 2;
const int LEFT = 3;
const int RIGHT = 4;

std::pair<int, int> findSaved(const Node s, const Node e, const std::vector<std::string>& lines) {
  auto cmp = [](const Node& a, const Node& b) -> bool {
    return a.steps > b.steps;
  };
  std::priority_queue<Node, std::vector<Node>, decltype(cmp)> pq(cmp);
  pq.push(s);
  std::set<std::pair<int, int>> visited;
  int bestSteps = INT_MAX;
  std::vector<Node> nodes;
  nodes.push_back(s);
  while (!pq.empty()) {
    const Node current = pq.top();
    pq.pop();
    if (current.x == e.x && current.y == e.y) {
      bestSteps = std::min(bestSteps, current.steps);
      continue;
    }
    std::tuple<int, int> directions[] = {
      {current.x, current.y - 1},
      {current.x, current.y + 1},
      {current.x - 1, current.y},
      {current.x + 1, current.y}
    };
    for (const auto& [x, y] : directions) {
      assertWithMessage(x >= 0 && x < lines[0].length() && y >= 0 && y < lines.size(),
        std::to_string(x) + ":" + std::to_string(y));

      if ((lines[y][x] == '.' || lines[y][x] == 'E') && visited.count({ x, y }) == 0) {
        Node newNode = Node{ x, y, current.steps + 1 } ;
        pq.push(newNode);
        nodes.push_back(newNode);
        visited.insert({ x, y });
      }
    }
  }
  int saved = 0;
  for (int i = 0; i < nodes.size(); ++i) {
    for (int j = i + 1; j < nodes.size(); ++j) {
      const Node& a = nodes[i];
      const Node& b = nodes[j];
      int man_dist = std::abs(a.x - b.x) + std::abs(a.y - b.y);
      if (man_dist == 2 && a.steps < b.steps) {
        // difference between the initial node with more steps and the
        // new node with less steps by "cheating"
        int diff = b.steps - (a.steps + 2);
        // if difference greater than or eq to 100 then
        // ++saved
        if (diff >= 100) {
          ++saved;
        }
      }
    }
  }
  int saved2 = 0;
  for (int i = 0; i < nodes.size(); ++i) {
    for (int j = i + 1; j < nodes.size(); ++j) {
      const Node& a = nodes[i];
      const Node& b = nodes[j];
      int man_dist = std::abs(a.x - b.x) + std::abs(a.y - b.y);
      if (man_dist <= 20 && a.steps < b.steps) {
        // difference between the initial node with more steps and the
        // new node with less steps by "cheating"
        int diff = b.steps - (a.steps + man_dist);
        if (diff >= 100) {
          ++saved2;
        }
      }
    }
  }
  return {saved, saved2};
}

int main() {
  std::string contents = readFile("day20.input");
  std::vector<std::string> lines = split(contents, "\n");
  Node s{ 0, 0, 0 };
  Node e{ 0, 0, 0 };
  for (int r = 0; r < lines.size(); ++r) {
    for (int c = 0; c < lines[r].length(); ++c) {
      if (lines[r][c] == 'S') {
        s.x = c;
        s.y = r;
      }
      if (lines[r][c] == 'E') {
        e.x = c;
        e.y = r;
      }
    }
  }

  const auto [saved, saved2] = findSaved(s, e, lines);
  std::cout << saved << std::endl;
  //1006575 too low
  std::cout << saved2 << std::endl;

  return 0;
}
