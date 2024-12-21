#include <iostream>
#include <fstream>
#include <stdexcept>
#include <queue>
#include <vector>
#include <string>
#include <set>

struct Node {
  int x;
  int y;
  int direction;
  int score;
  std::set<std::pair<int, int>> path;
};

bool operator==(const Node& a, const Node& b) {
  return a.x == b.x && a.y == b.y && a.direction == b.direction && a.score == b.score;
}

std::ostream& operator<<(std::ostream& s, const Node& n) {
  return s << n.x << " " << n.y << " " << n.direction << " " << n.score << "\n";
}

const int UP = 0;
const int DOWN = 1;
const int RIGHT = 2;
const int LEFT = 3;

int main(void) {
  std::ifstream input("day16.input");
  if (!input.is_open()) {
    throw std::runtime_error("input file cannot be opened");
  }
  auto compare = [](const Node& a, const Node& b) {
    return a.score > b.score;
  };
  std::string line;
  std::vector<std::string> matrix;
  while (input >> line) {
    matrix.push_back(line);
  }
  std::set<std::pair<int, int>> walls;
  std::priority_queue<Node, std::vector<Node>, decltype(compare)> pq(compare);
  Node s{-1, -1, RIGHT, 0, std::set<std::pair<int, int>>{}};
  Node e{-1, -1, RIGHT, 0, std::set<std::pair<int, int>>{}};
  for (int i = 0; i < matrix.size(); ++i) {
    for (int j = 0; j < matrix[i].length(); ++j) {
      if (matrix[i][j] == '#') {
        walls.insert({j, i});
      }
      if (matrix[i][j] == 'S') {
        s.x = j;
        s.y = i;
      }
      if (matrix[i][j] == 'E') {
        e.x = j;
        e.y = i;
      }
    }
  }
  std::cout << s;
  std::cout << e;
  std::cout << "\n";
  std::set<std::tuple<int, int, int>> visited;
  pq.push(s);
  int minScore = -1;
  std::set<std::pair<int, int>> uniquePointsForBestPaths;
  while (pq.size() > 0) {
    Node current = pq.top();
    pq.pop();
    visited.insert({current.x, current.y, current.direction});
    std::cout << current;
    if (current.x == e.x && current.y == e.y) {
      if (minScore == -1) {
        minScore = current.score;
      }
      if (minScore == current.score) {
        std::cout << "found\n";
        uniquePointsForBestPaths.merge(current.path);
      }
    }
    if (walls.count({ current.x, current.y - 1 }) == 0 &&
      visited.count({ current.x, current.y - 1, UP }) == 0) {
      Node upNode{ current.x, current.y - 1, UP, 0, current.path };
      if (current.direction != upNode.direction) {
        upNode.score = current.score + 1001;
      } else {
        upNode.score = current.score + 1;
      }
      upNode.path.insert({ current.x, current.y - 1 });
      pq.push(upNode);
    }
    if (walls.count({ current.x, current.y + 1 }) == 0 &&
      visited.count({ current.x, current.y + 1, DOWN }) == 0) {
      Node downNode{ current.x, current.y + 1, DOWN, 0, current.path };
      if (current.direction != downNode.direction) {
        downNode.score = current.score + 1001;
      } else {
        downNode.score = current.score + 1;
      }
      downNode.path.insert({ current.x, current.y + 1 });
      pq.push(downNode);
    }
    if (walls.count({ current.x - 1, current.y }) == 0 &&
      visited.count({ current.x - 1, current.y, LEFT }) == 0) {
      Node leftNode{ current.x - 1, current.y, LEFT, 0, current.path };
      if (current.direction != leftNode.direction) {
        leftNode.score = current.score + 1001;
      } else {
        leftNode.score = current.score + 1;
      }
      leftNode.path.insert({ current.x - 1, current.y });
      pq.push(leftNode);
    }
    if (walls.count({ current.x + 1, current.y }) == 0 &&
      visited.count({ current.x + 1, current.y, RIGHT }) == 0) {
      Node rightNode{ current.x + 1, current.y, RIGHT, 0, current.path };
      if (current.direction != rightNode.direction) {
        rightNode.score = current.score + 1001;
      } else {
        rightNode.score = current.score + 1;
      }
      rightNode.path.insert({ current.x + 1, current.y });
      pq.push(rightNode);
    }
  }
  std::cout << uniquePointsForBestPaths.size() + 1 << std::endl; // adding one because of E
  return 0;
}
