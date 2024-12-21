#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <fstream>
#include <stdexcept>
#include <vector>
#include <string>
struct Computer {
  int64_t a = 0;
  int64_t b = 0;
  int64_t c = 0;
  std::vector<int> program;
  size_t instruction_pointer = 0;
  std::vector<int> output_buffer;
  int64_t getOperandValue(int operand) {
    switch (operand) {
      case 0:
      case 1:
      case 2:
      case 3: {
        return operand;
      } break;
      case 4: {
        return a;
      } break;
      case 5: {
        return b;
      } break;
      case 6: {
        return c;
      } break;
      case 7: {
        throw std::runtime_error("OPERAND VALUE 7");
      } break;
      default: throw std::runtime_error("UNHANDLED COMBO OPERAND");
    }
  }
  void process(int opcode, int operand) {
    switch (opcode) {
      case 0: {
        a = a / (int64_t(1) << getOperandValue(operand));
        instruction_pointer += 2;
      } break;
      case 1: {
        b = b ^ operand;
        instruction_pointer += 2;
      } break;
      case 2: {
        b = getOperandValue(operand) & 7;
        instruction_pointer += 2;
      } break;
      case 3: {
        if (a == 0) {
          instruction_pointer += 2;
          return;
        }
        instruction_pointer = operand;
        assert(instruction_pointer < program.size());
      } break;
      case 4: {
        b = b ^ c;
        instruction_pointer += 2;
      } break;
      case 5: {
        output_buffer.push_back(getOperandValue(operand) & 7);
        instruction_pointer += 2;
      } break;
      case 6: {
        b = a / (int64_t(1) << getOperandValue(operand));
        instruction_pointer += 2;
      } break;
      case 7: {
        c = a / (int64_t(1) << getOperandValue(operand));
        instruction_pointer += 2;
      } break;
      default: throw std::runtime_error("UNHANDLED OPCODE");
    }
  }
  void run() {
    while (instruction_pointer < program.size()) {
      process(program[instruction_pointer], program[instruction_pointer + 1]);
    }
  }
};
std::ostream& operator<<(std::ostream& os, std::vector<int> v) {
  for (const int& n : v) {
    os << n << " ";
  }
  return os << "\n";
}
int main(void) {
  std::ifstream input("day17.input");
  if (!input.is_open()) {
    throw std::runtime_error("input file cannot be opened");
  }
  std::string line;
  std::vector<int> register_values;
  std::vector<int> program;
  while (!input.eof()) {
    std::getline(input, line);
    if (line.find("Register") != std::string::npos) {
      int colonIdx = line.find(": ");
      std::string numStr = line.substr(colonIdx + 2);
      int value = std::stoi(numStr);
      register_values.push_back(value);
    }
    if (line.find("Program") != std::string::npos) {
      int colonIdx = line.find(": ");
      std::string programStr = line.substr(colonIdx + 2);
      for (int i = 0; i < programStr.length(); ++i) {
        if (i % 2 != 0) continue;
        program.push_back(programStr[i] - '0');
      }
    }
  }
  Computer computer{ register_values[0], register_values[1], register_values[2], program };
  computer.run();
  std::cout << computer.output_buffer;
  return 0;
}
