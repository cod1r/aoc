#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <fstream>
#include <stdexcept>
#include <vector>
#include <string>
#include <set>
#include <queue>

struct Node {
  int x;
  int y;
  int steps;
};

const int UP = 0;
const int DOWN = 1;
const int LEFT = 2;
const int RIGHT = 3;

const int NUM_ROWS = 71;
const int NUM_COLS = 71;
const int NUM_SIMULATED = 1024;

bool findPath(const std::vector<std::string>& matrix) {
  Node start{ 0, 0, 0 };
  std::set<std::tuple<int, int>> visited;
  visited.insert({ 0, 0 });
  auto cmp = [](const Node& a, const Node& b) -> bool {
    /*const int a_man_dist = (NUM_ROWS - 1 - a.y) + (NUM_COLS - 1 - a.x);*/
    /*const int b_man_dist = (NUM_ROWS - 1 - b.y) + (NUM_COLS - 1 - b.x);*/
    /*return a_man_dist > b_man_dist;*/
    return a.steps > b.steps;
  };
  std::priority_queue<Node, std::vector<Node>, decltype(cmp)> pq(cmp);
  pq.push(start);
  while (!pq.empty()) {
    const Node current = pq.top();
    assert(current.y >= 0 && current.y < NUM_ROWS && current.x >= 0 && current.x < NUM_COLS && matrix[current.y][current.x] != '#');
    pq.pop();
    if (current.x == NUM_COLS - 1 && current.y == NUM_ROWS - 1) {
      std::cout << current.steps << std::endl;
      return true;
    }
    std::tuple<int, int> directions[] = {
      { current.x, current.y - 1 },
      { current.x, current.y + 1 },
      { current.x - 1, current.y },
      { current.x + 1, current.y }
    };
    for (const auto& [x, y] : directions) {
      if (visited.count({ x, y }) == 0 &&
        x >= 0 && x < NUM_COLS &&
        y >= 0 && y < NUM_ROWS &&
        matrix[y][x] != '#') {
        pq.push(Node{ x, y, current.steps + 1 });
        visited.insert({ x, y });
      }
    }
  }
  return false;
}

int main(void) {
  std::ifstream input("day18.input");
  if (!input.is_open()) {
    throw std::runtime_error("input file cannot be opened");
  }
  std::vector<std::pair<int, int>> coords;
  std::string line;
  while (!input.eof()) {
    std::getline(input, line);
    int commaIdx = line.find(",");
    if (commaIdx == -1) continue;
    std::string firstNumStr = line.substr(0, commaIdx);
    std::string secondNumStr = line.substr(commaIdx + 1);
    coords.push_back({ std::stoi(firstNumStr), std::stoi(secondNumStr) });
  }

  std::vector<std::string> matrix(NUM_ROWS, std::string(NUM_COLS, '.'));
  for (int i = 0; i < NUM_SIMULATED; ++i) {
    const auto& [x, y] = coords[i];
    matrix[y][x] = '#';
  }
  findPath(matrix);

  for (int i = NUM_SIMULATED + 1; i < coords.size(); ++i) {
    const auto& [x, y] = coords[i];
    matrix[y][x] = '#';
    const auto& found = findPath(matrix);
    if (!found) {
      std::cout << x << " " << y << std::endl;
      break;
    }
  }

  return 0;
}
