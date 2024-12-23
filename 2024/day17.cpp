#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <fstream>
#include <stdexcept>
#include <vector>
#include <string>
struct Computer {
  uint64_t a = 0;
  uint64_t b = 0;
  uint64_t c = 0;
  std::vector<int> program;
  size_t instruction_pointer = 0;
  std::vector<int> output_buffer;
  uint64_t getOperandValue(int operand) {
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
  void process(uint64_t opcode, uint64_t operand) {
    switch (opcode) {
      case 0: {
        a = a >> getOperandValue(operand);
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
        b = a >> getOperandValue(operand);
        instruction_pointer += 2;
      } break;
      case 7: {
        c = a >> getOperandValue(operand);
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

void brute_force(uint64_t a, const std::vector<int>& program, int program_index);
int main(void) {
  std::ifstream input("day17.input");
  if (!input.is_open()) {
    throw std::runtime_error("input file cannot be opened");
  }
  std::string line;
  std::vector<uint64_t> register_values;
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
  for (uint64_t i = 0; i <= (1ull << 16); ++i) {
    brute_force(i, program, program.size() - 1);
  }
  return 0;
}

void brute_force(uint64_t a, const std::vector<int>& program, int program_index) {
  for (int bits = 0; bits < 8; ++bits) {
    uint64_t b = (bits & 7) ^ 1;
    uint64_t c = (a | bits) >> b;
    uint64_t result = ((b^4)^c) & 7;
    if (result == program[program_index]) {
      uint64_t newA = a | bits;
      if (program_index > 0) {
        brute_force(newA << 3, program, program_index - 1);
      } else {
        Computer c{ newA, 0, 0, program };
        c.run();
        if (c.output_buffer == program) {
          std::cout << newA << std::endl;
          std::cout << c.output_buffer;
          std::cout << program;
          std::cout << std::endl;
        }
      }
    }
  }
}


/*
 * 2,4,1,1,7,5,0,3,1,4,4,4,5,5,3,0
 *
 * register B = register A & 7
 * register B = register B ^ 1
 * register C = A / 2^B
 * register A = A / 2^3
 * register B = register B ^ 4
 * register B = B ^ C
 * output B & 7
 * set instruction_pointer to 0 if A not 0. else move instruction_pointer + 2
 *
 * B = (A & 7) ^ 1
 * C = A >> B
 * B = ((B ^ 4) ^ C) & 7
 * output B
 * A = A >> 3
 *
 * 010
 * 011
 * 101
 */
