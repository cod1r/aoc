#include "../aoc_utils.h"
#include <bitset>
#include <iterator>
#include <set>
#include <map>
#include <algorithm>
#include <cassert>
#include <stdexcept>
#include <string>
#include <tuple>
#include <utility>
#include <unordered_set>

int AND(int a, int b) {
  assert(a <= 1);
  assert(b <= 1);
  return a & b;
}
int XOR(int a, int b) {
  assert(a <= 1);
  assert(b <= 1);
  return a ^ b;
}
int OR(int a, int b) {
  assert(a <= 1);
  assert(b <= 1);
  return a | b;
}

int main(void) {
  std::string contents = readFile("day24.input");
  std::vector<std::string> lines = split(contents, "\n");
  if (lines.back() == "") lines.pop_back();
  std::map<std::string, int64_t> values;
  for (const auto& s : lines) {
    if (s == "") break;
    auto idxColon = s.find(":");
    if (idxColon != std::string::npos) {
      std::string wire = s.substr(0, idxColon);
      std::string value = s.substr(idxColon + 2, s.size() - (idxColon + 2));
      values.insert({ wire, std::stoi(value) });
    }
  }
  int start = -1;
  for (int i = 0; i < lines.size(); ++i) {
    if (lines[i] == "") {
      start = i + 1;
      break;
    }
  }
  assert(start != -1);
  std::vector<std::pair<std::tuple<std::string, std::string, std::string>, std::string>> graph;
  for (;start < lines.size(); ++start) {
    const auto& s = lines[start];
    std::vector<std::string> splitted = split(s, " ");
    graph.push_back({ { splitted[0], splitted[2], splitted[1] }, splitted[4] });
  }
  std::function<bool(std::map<std::string, int64_t>&)> loop = [&graph](std::map<std::string, int64_t>& values) {
    bool again = false;
    for (const auto& [i, o] : graph) {
      const auto& [w1, w2, op] = i;
      if (values.count(w1) == 0 || values.count(w2) == 0) {
        assertWithMessage(values.count(o) == 0, w1 + ":" + w2);
        again = true;
        continue;
      }
      if (values.count(o)) {
        continue;
      }

      if (op == "AND") {
        values.insert({ o, AND(values[w1], values[w2]) });
      }
      if (op == "XOR") {
        values.insert({ o, XOR(values[w1], values[w2]) });
      }
      if (op == "OR") {
        values.insert({ o, OR(values[w1], values[w2]) });
      }
    }
    return again;
  };
  auto copy_values = values;
  while (loop(copy_values));

  for (const auto& [i, o] : graph) {
    assert(copy_values.count(o) > 0);
  }
  for (const auto& [i, o] : copy_values) {
    assert(o <= 1);
  }

  std::vector<std::string> z;
  for (const auto& [w, _] : copy_values) {
    if (w[0] == 'z') z.push_back(w);
  }
  auto cmp = [](const auto& a, const auto& b) {
    const int an = std::stoi(a.substr(1));
    const int bn = std::stoi(b.substr(1));
    return an < bn;
  };
  std::sort(z.begin(), z.end(), cmp);
  auto getZ = [&z](const std::map<std::string, int64_t>& cvals) {
      int bit = 0;
      int64_t potential = 0;
      for (const auto& s : z) {
        if (cvals.count(s) == 0) break;
        potential |= (cvals.at(s) << bit);
        ++bit;
      }
      return potential;
  };
  int64_t ans = getZ(copy_values);
  //1292512718 too low
  std::cout << ans << std::endl;

  struct Gate{ std::string a = ""; std::string b = ""; std::string op = ""; std::string output = ""; };
  struct Adder{
    std::string type;
    Gate A_B_XOR;
    Gate A_B_AND;
    Gate A_B_XOR_XOR_Cin;
    Gate A_B_XOR_AND_Cin;
    Gate A_B_XOR_AND_Cin_OR_A_B_AND;
  };

  std::vector<Adder> adders;
  //div by 2 because only we only need number of bits not
  //num of bits * 2
  //due to there being both x and y bits values in values
  for (int i = 0; i < values.size() / 2; ++i) {
    Adder adder { i == 0 ? "HALF" : "FULL" };
    // getting first two gates that directly take in x and y bits
    for (const auto& [t, o] : graph) {
      const auto& [i1, i2, op] = t;
      if (i1[0] == 'x' || i1[0] == 'y') {
        int bitpos = std::stoi(i1.substr(1));
        if (bitpos == i) {
          if (op == "XOR") adder.A_B_XOR = {i1, i2, op, o};
          if (op == "AND") adder.A_B_AND = {i1, i2, op, o};
        }
      }
    }

    if (adder.type == "FULL") {
      const auto& prev_adder = adders[i - 1];
      // getting the gate that XOR's the result of A XOR B with Carry input and
      for (const auto& [t, o] : graph) {
        const auto& [i1, i2, op] = t;
        if (op == "XOR" && (i1 == adder.A_B_XOR.output || i2 == adder.A_B_XOR.output ||
            (prev_adder.type == "HALF" && (i1 == prev_adder.A_B_AND.output || i2 == prev_adder.A_B_AND.output)) ||
            (i1 == prev_adder.A_B_XOR_AND_Cin_OR_A_B_AND.output || i2 == prev_adder.A_B_XOR_AND_Cin_OR_A_B_AND.output))) {
          adder.A_B_XOR_XOR_Cin = {i1, i2, op, o};
        }
      }

      // getting the gate that AND's the result of A XOR B with Carry input
      for (const auto& [t, o] : graph) {
        const auto& [i1, i2, op] = t;
        if (op == "AND" && (i1 == adder.A_B_XOR.output || i2 == adder.A_B_XOR.output ||
            (prev_adder.type == "HALF" && (i1 == prev_adder.A_B_AND.output || i2 == prev_adder.A_B_AND.output)) ||
            (i1 == prev_adder.A_B_XOR_AND_Cin_OR_A_B_AND.output || i2 == prev_adder.A_B_XOR_AND_Cin_OR_A_B_AND.output))) {
          adder.A_B_XOR_AND_Cin = {i1, i2, op, o};
        }
      }

      // getting the OR gate
      for (const auto& [t, o] : graph) {
        const auto& [i1, i2, op] = t;
        if (op == "OR" && (i1 == adder.A_B_XOR_AND_Cin.output || i2 == adder.A_B_XOR_AND_Cin.output ||
          i1 == adder.A_B_AND.output || i2 == adder.A_B_AND.output)) {
          adder.A_B_XOR_AND_Cin_OR_A_B_AND = {i1, i2, op, o};
        }
      }
    }
    adders.push_back(adder);
  }
  std::cout<<"size: " <<adders.size()<<std::endl;

  std::vector<std::string> wrongs;
  for (int i = 0; i < adders.size(); ++i) {
    auto adder = adders[i];
    if (adder.type == "HALF") {
      if (adder.A_B_XOR.output[0] != 'z' || std::stoi(adder.A_B_XOR.output.substr(1)) != i) {
        wrongs.push_back(adder.A_B_XOR.output);
      }
      auto next_adder = adders[i + 1];
      if (adder.A_B_AND.output != next_adder.A_B_XOR_XOR_Cin.a &&
        adder.A_B_AND.output != next_adder.A_B_XOR_XOR_Cin.b) {
        std::cout << "WHAT: " << next_adder.A_B_XOR_XOR_Cin.a << " " << next_adder.A_B_XOR_XOR_Cin.b << std::endl;
        wrongs.push_back(adder.A_B_AND.output);
      }
      continue;
    }

    if (adder.A_B_AND.output != adder.A_B_XOR_AND_Cin_OR_A_B_AND.a &&
      adder.A_B_AND.output != adder.A_B_XOR_AND_Cin_OR_A_B_AND.b) {
      if (adder.A_B_AND.output.size()) {
        wrongs.push_back(adder.A_B_AND.output);
      }
    }
    if (adder.A_B_XOR.output != adder.A_B_XOR_XOR_Cin.a &&
      adder.A_B_XOR.output != adder.A_B_XOR_XOR_Cin.b) {
      if (adder.A_B_XOR.output.size()) {
        wrongs.push_back(adder.A_B_XOR.output);
      }
    }
    if (adder.A_B_XOR_XOR_Cin.output[0] != 'z' || std::stoi(adder.A_B_XOR_XOR_Cin.output.substr(1)) != i) {
      if (adder.A_B_XOR_XOR_Cin.output.size()) {
        wrongs.push_back(adder.A_B_XOR_XOR_Cin.output);
      }
    }
    if (adder.A_B_XOR_AND_Cin.output != adder.A_B_XOR_AND_Cin_OR_A_B_AND.a &&
      adder.A_B_XOR_AND_Cin.output != adder.A_B_XOR_AND_Cin_OR_A_B_AND.b) {
      if (adder.A_B_XOR_AND_Cin.output.size()) {
        wrongs.push_back(adder.A_B_XOR_AND_Cin.output);
      }
    }
    if (i + 1 < adders.size()) {
      auto next_adder = adders[i + 1];
      if (adder.A_B_XOR_AND_Cin_OR_A_B_AND.output != next_adder.A_B_XOR_XOR_Cin.a &&
        adder.A_B_XOR_AND_Cin_OR_A_B_AND.output != next_adder.A_B_XOR_XOR_Cin.b) {
        if (adder.A_B_XOR_AND_Cin_OR_A_B_AND.output.size()) {
          wrongs.push_back(adder.A_B_XOR_AND_Cin_OR_A_B_AND.output);
        }
      }
    } else {
      if (adder.A_B_XOR_AND_Cin_OR_A_B_AND.output[0] != 'z' ||
        std::stoi(adder.A_B_XOR_AND_Cin_OR_A_B_AND.output.substr(1)) != i + 1) {
          wrongs.push_back(adder.A_B_XOR_AND_Cin_OR_A_B_AND.output);
      }
    }
  }
  std::sort(wrongs.begin(), wrongs.end());
  std::cout << join(wrongs, ",") << std::endl;
  //jgb,rkf,rrs,rvc,vcg,z09,z20,z24 right? - RIGHT!!!!!!!!!!!!!!!!!!

  // below code doesn't work. realized that my tests to assert whether
  // the wires produce a valid addition system is wrong. (probably due to some edge case)
  /*std::vector<std::string> x;*/
  /*std::vector<std::string> y;*/
  /*for (const auto& [w, _] : values) {*/
  /*  if (w[0] == 'x') x.push_back(w);*/
  /*  if (w[0] == 'y') y.push_back(w);*/
  /*}*/
  /**/
  /*std::sort(x.begin(), x.end(), cmp);*/
  /*std::sort(y.begin(), y.end(), cmp);*/
  /*auto getX = [&x](const std::map<std::string, int64_t>& cvals) {*/
  /*    int bit = 0;*/
  /*    int64_t potential = 0;*/
  /*    for (const auto& s : x) {*/
  /*      potential |= (cvals.at(s) << bit);*/
  /*      ++bit;*/
  /*    }*/
  /*    return potential;*/
  /*};*/
  /*auto getY = [&y](const std::map<std::string, int64_t>& cvals) {*/
  /*    int bit = 0;*/
  /*    int64_t potential = 0;*/
  /*    for (const auto& s : y) {*/
  /*      potential |= (cvals.at(s) << bit);*/
  /*      ++bit;*/
  /*    }*/
  /*    return potential;*/
  /*};*/
  /*int64_t x_num = getX(copy_values);*/
  /**/
  /*int64_t y_num = getY(copy_values);*/
  /**/
  /*int64_t z_num = x_num + y_num;*/
  /*std::cout << z_num << std::endl;*/
  /**/
  /*std::function<bool(int64_t, std::map<std::string, int64_t>)> runLoopAndCheckIsBroken =*/
  /*[&loop, &z, &getZ](int64_t assert_value, std::map<std::string, int64_t> cvals) {*/
  /*    size_t old_len = cvals.size();*/
  /*    while (loop(cvals)) {*/
  /*      if (old_len == cvals.size()) break;*/
  /*      old_len = cvals.size();*/
  /*    }*/
  /*    int64_t potential = getZ(cvals);*/
  /*    return potential != assert_value;*/
  /*};*/
  /*std::function<int64_t(std::map<std::string, int64_t>)> runLoopAndGetZ = [&loop, &z, &getZ](std::map<std::string, int64_t> cvals) {*/
  /*    size_t old_len = cvals.size();*/
  /*    while (loop(cvals)) {*/
  /*      if (old_len == cvals.size()) break;*/
  /*      old_len = cvals.size();*/
  /*    }*/
  /*    int64_t potential = getZ(cvals);*/
  /*    return potential;*/
  /*};*/
  /*std::function<std::vector<std::string>(std::map<std::string, int64_t>)> runLoopAndGetOnes =*/
  /*  [&loop, &z, &getZ](std::map<std::string, int64_t> cvals) {*/
  /*    size_t old_len = cvals.size();*/
  /*    std::vector<std::string> ones;*/
  /*    while (loop(cvals)) {*/
  /*      for (const auto& [f, s] : cvals) {*/
  /*        if (s) ones.push_back(f);*/
  /*      }*/
  /*      if (old_len == cvals.size()) break;*/
  /*      old_len = cvals.size();*/
  /*    }*/
  /*    return ones;*/
  /*};*/
  /*auto runChecks = [&runLoopAndCheckIsBroken, &x](std::map<std::string, int64_t> custom_values, std::string s) {*/
  /*  bool broken = false;*/
  /*  {*/
  /*    custom_values[s] = 1;*/
  /*    int64_t bit_shift_value = std::stoll(s.substr(1));*/
  /*    int64_t assert_value = 1LL << bit_shift_value;*/
  /*    broken = runLoopAndCheckIsBroken(assert_value, custom_values);*/
  /*    std::vector pairs(custom_values.begin(), custom_values.end());*/
  /*    for (const auto& [defined, _] : pairs) {*/
  /*      if (defined[0] != 'x' && defined[0] != 'y')*/
  /*        custom_values.erase(defined);*/
  /*    }*/
  /*    custom_values[s] = 0;*/
  /*  }*/
  /*  {*/
  /*    custom_values[s] = 1;*/
  /*    auto scopy= s;*/
  /*    scopy[0] = 'y';*/
  /*    custom_values[scopy] = 1;*/
  /*    int64_t bit_shift_value = std::stoll(s.substr(1));*/
  /*    int64_t assert_value = 1LL << (bit_shift_value + 1);*/
  /*    broken = runLoopAndCheckIsBroken(assert_value, custom_values);*/
  /*    std::vector pairs(custom_values.begin(), custom_values.end());*/
  /*    for (const auto& [defined, _] : pairs) {*/
  /*      if (defined[0] != 'x' && defined[0] != 'y')*/
  /*        custom_values.erase(defined);*/
  /*    }*/
  /*    custom_values[s] = 0;*/
  /*    custom_values[scopy] = 0;*/
  /*  }*/
  /*  return broken;*/
  /*};*/
  /*auto pairs = generatePairsIndices(graph.size());*/
  /*std::vector<std::set<std::pair<int, int>>> partitions;*/
  /*for (const auto& p : pairs) {*/
  /*  std::set<std::pair<int, int>> group{ p };*/
  /*  std::unordered_set<int> n{ p.first, p.second };*/
  /*  for (const auto& p2 : pairs) {*/
  /*    if (n.count(p2.first) == 0 && n.count(p2.second) == 0) {*/
  /*      group.insert(p2);*/
  /*      n.insert(p2.first);*/
  /*      n.insert(p2.second);*/
  /*    }*/
  /*  }*/
  /*  partitions.push_back(group);*/
  /*}*/
  /*std::cout << partitions.size() << std::endl;*/
  /*for (const auto& partition : partitions) {*/
  /*  for (const auto& p : partition) {*/
  /*    std::cout << p << std::endl;*/
  /*  }*/
  /*}*/
  /*std::map<std::string, int64_t> custom_values;*/
  /*for (const auto& s : y) {*/
  /*  custom_values.insert({s, 0});*/
  /*}*/
  /*for (const auto& s : x) {*/
  /*  custom_values.insert({s, 0});*/
  /*}*/
  /*auto allBitsBefore = [&](int until) {*/
  /*  for (int i = 0 ; i <= until; ++i) {*/
  /*    const auto& s = x[i];*/
  /*    auto b = runChecks(custom_values, s);*/
  /*    if (b) {*/
  /*      return false;*/
  /*    }*/
  /*  }*/
  /*  {*/
  /*  auto ccustom = custom_values;*/
  /*  for (int i = 0 ; i <= until; ++i) {*/
  /*    const auto& s = x[i];*/
  /*    ccustom[s] = 1;*/
  /*  }*/
  /*  ccustom[y[0]] = 1;*/
  /*  auto v = runLoopAndGetZ(ccustom);*/
  /*  if (v != (1LL << (until + 1))) {*/
  /*    return false;*/
  /*  }*/
  /*  }*/
  /*  {*/
  /*    auto ccustom = custom_values;*/
  /*    for (int i = 0 ; i <= until; ++i) {*/
  /*      const auto& s = x[i];*/
  /*      ccustom[s] = 1;*/
  /*      ccustom[y[i]] = 1;*/
  /*    }*/
  /*    int64_t val = ((1LL << (until + 1)) - 1) * 2;*/
  /*    auto v = runLoopAndGetZ(ccustom);*/
  /*    if (v != val) {*/
  /*      return false;*/
  /*    }*/
  /*  }*/
  /*  return true;*/
  /*};*/

  // code below is what tries a bunch of swaps according to tests that
  // are good enough to guage if the wires are connected correctly so that
  // addition occurs
  /*std::vector<std::pair<int, int>> good;*/
  /*std::unordered_set<int> used;*/
  /*for (int bit_idx = 0; bit_idx < x.size(); ++bit_idx) {*/
  /*  const auto& s = x[bit_idx];*/
  /*  auto b = runChecks(custom_values, s);*/
  /*  if (b) {*/
  /*    auto try_first_good_swap = [&]() {*/
  /*      for (int i = 0; i < graph.size(); ++i) {*/
  /*        if (used.count(i)) continue;*/
  /*        for (int j = i + 1; j < graph.size(); ++j) {*/
  /*          if (used.count(j)) continue;*/
  /*          std::swap(std::get<1>(graph[i]), std::get<1>(graph[j]));*/
  /*          auto valid = allBitsBefore(bit_idx);*/
  /*          if (valid) {*/
  /*            std::cout << i << " " << j << " " << s << std::endl;*/
  /*            good.push_back({i, j});*/
  /*            used.insert(i);*/
  /*            used.insert(j);*/
  /*            return;*/
  /*          }*/
  /*          std::swap(std::get<1>(graph[i]), std::get<1>(graph[j]));*/
  /*        }*/
  /*      }*/
  /*      throw std::runtime_error("couldn't find good swap");*/
  /*    };*/
  /*    try_first_good_swap();*/
  /*  }*/
  /*}*/
  /*for (const auto& [f, s] : good) {*/
  /*  std::swap(std::get<1>(graph[f]), std::get<1>(graph[s]));*/
  /*}*/
  /*auto v = runLoopAndGetZ(custom_values);*/
  /*assert(v == z_num);*/
      /*std::vector<std::string> outputs;*/
      /*for (const auto& p : pairs_used) {*/
      /*  outputs.push_back(std::get<1>(graph[p.first]));*/
      /*  outputs.push_back(std::get<1>(graph[p.second]));*/
      /*}*/
      /*std::sort(outputs.begin(), outputs.end());*/
      /*std::cout << join(outputs, ",") << std::endl;*/
//7 181 x08
//39 60 x19
//3 66 x20
//13 80 x23
//78 135 x31
//jgb,kmm,vcg,z09,z20,z24,z31,z32 not right
//jgb,mfq,vcg,z09,z10,z20,z24,z31 not right
//
//maybe right(NOT RIGHT)
//53 181 x08
//60 66 x19
//13 80 x23
//135 186 x31
//NOPE
  return 0;
}

  // most brute force attempt: needs to pick a group of 8 out of all of the pairs
  /*auto pairs = generatePairsIndices(graph.size());*/
  /*      std::cout << pairs.size() << std::endl;*/
  /*      for (const auto [f, s] : pairs) {*/
  /*        std::vector<std::pair<int, int>> custom_group{ {f, s} };*/
  /*        for (const auto [inf, ins] : pairs) {*/
  /*          const int infc = inf;*/
  /*          const int insc = ins;*/
  /*          auto found = std::find_if(custom_group.begin(), custom_group.end(),*/
  /*          [&infc, &insc](const auto& p) {*/
  /*            return infc == p.first || insc == p.second || infc == p.second || insc == p.first;*/
  /*          });*/
  /*          if (found == custom_group.end()) {*/
  /*            custom_group.push_back({inf, ins});*/
  /*          }*/
  /*        }*/
  /*        std::cout << "cg: " << custom_group.size() << std::endl;*/
  /*          for (const auto& [f, s]: custom_group) {*/
  /*            auto temp1 = std::get<1>(graph[f]);*/
  /*            auto temp2 = std::get<1>(graph[s]);*/
  /*            std::get<1>(graph[f]) = temp2;*/
  /*            std::get<1>(graph[s]) = temp1;*/
  /*          }*/
  /*          auto copy_values = values;*/
  /*          while (loop(copy_values));*/
  /*          auto potential = getZ(copy_values);*/
  /*          if (potential == z_num) throw 1;*/
  /*          for (const auto& [f, s]: custom_group) {*/
  /*            auto temp1 = std::get<1>(graph[f]);*/
  /*            auto temp2 = std::get<1>(graph[s]);*/
  /*            std::get<1>(graph[f]) = temp2;*/
  /*            std::get<1>(graph[s]) = temp1;*/
  /*          }*/
  /*      }*/



/*void getForwardPath(const std::vector<std::pair<std::tuple<std::string, std::string, std::string>, std::string>>& graph,*/
/*const std::string& wire, std::set<std::tuple<std::string, std::string, std::string, std::string>>& some) {*/
/*  if (wire.find("z") != std::string_view::npos) {*/
/*    return;*/
/*  }*/
/*  for (const auto& [t, o]: graph) {*/
/*    const auto& [i1, i2, op] = t;*/
/*    if ((i1 == wire || i2 == wire) && some.count({ i1, i2, op, o }) == 0) {*/
/*      some.insert({i1, i2, op, o});*/
/*      getForwardPath(graph, o, some);*/
/*    }*/
/*  }*/
/*}*/
/*void printPath2(const std::vector<std::pair<std::tuple<std::string, std::string, std::string_view>, std::string>>& graph,*/
/*std::string wire, std::set<std::pair<std::tuple<std::string, std::string, std::string_view>, std::string>>& visited) {*/
/*  if (wire[0] == 'z') {*/
/*    std::cout << wire << std::endl;*/
/*    return;*/
/*  }*/
/*  for (const auto& [t, o]: graph) {*/
/*    const auto& [i1, i2, op] = t;*/
/*    if (visited.count({ t, o }) == 0) {*/
/*    if (i2 == wire || i1 == wire) {*/
/*      std::cout << i1 << " " << op << " " << i2 << std::endl;*/
/*      visited.insert({ t, o });*/
/*      printPath2(graph, i2, visited);*/
/*      printPath2(graph, i1, visited);*/
/*      printPath2(graph, o, visited);*/
/*    }*/
/*    }*/
/*  }*/
/*}*/
/*void printPath(const std::vector<std::pair<std::tuple<std::string, std::string, std::string_view>, std::string>>& graph,*/
/*std::string wire) {*/
/*  if (wire[0] == 'z') {*/
/*    std::cout << wire << std::endl;*/
/*    return;*/
/*  }*/
/*  for (const auto& [t, o]: graph) {*/
/*    const auto& [i1, i2, op] = t;*/
/*    if (i2 == wire || i1 == wire) {*/
/*      std::cout << i1 << " " << op << " " << i2 << std::endl;*/
/*      printPath(graph, o);*/
/*    }*/
/*  }*/
/*}*/
/*void printPathBackwards(const std::vector<std::pair<std::tuple<std::string, std::string, std::string_view>, std::string>>& graph,*/
/*std::string wire) {*/
/*  if (wire[0] == 'x' || wire[0] == 'y') {*/
/*    return;*/
/*  }*/
/*  for (const auto& [t, o]: graph) {*/
/*    const auto& [i1, i2, op] = t;*/
/*    if (o == wire || o == wire) {*/
/*      std::cout << i1 << " " << op << " " << i2 << std::endl;*/
/*      printPathBackwards(graph, i1);*/
/*      printPathBackwards(graph, i2);*/
/*    }*/
/*  }*/
/*}*/
/**/
/*void backtrack(const std::function<bool(int64_t, std::map<std::string, int64_t>)>& isBroken,*/
/*const std::vector<std::pair<int, int>>& pairs,*/
/*const std::function<int64_t(std::map<std::string, int64_t>)>& loopAndGetZ,*/
/*const int64_t& target, std::set<std::pair<int, int>> swappings,*/
/*const std::map<std::string, int64_t>& values,*/
/*std::vector<std::pair<std::tuple<std::string, std::string, std::string>, std::string>>& graph,*/
/*int64_t closest_value) {*/
/*  std::cout << target << " " << closest_value << std::endl;*/
/*  if (swappings.size() == 4) {*/
/*    auto broken = isBroken(target, values);*/
/*    if (!broken) {*/
/*      std::vector<std::string> outputs;*/
/*      for (const auto& p : swappings) {*/
/*        outputs.push_back(std::get<1>(graph[p.first]));*/
/*        outputs.push_back(std::get<1>(graph[p.second]));*/
/*      }*/
/*      std::sort(outputs.begin(), outputs.end());*/
/*      std::cout << join(outputs, ",") << std::endl;*/
/*      exit(0);*/
/*    }*/
/*    return;*/
/*  }*/
/*  for (const auto& p : pairs) {*/
/*    auto found = std::find_if(swappings.begin(), swappings.end(), [&p](const auto& sp) {*/
/*      return p.first == sp.first || p.first ==sp.second || p.second == sp.first || p.second == sp.second;*/
/*    });*/
/*    if (found == swappings.end()) {*/
/*      std::swap(std::get<1>(graph[p.first]), std::get<1>(graph[p.second]));*/
/*      auto v = loopAndGetZ(values);*/
/*      auto diff = std::abs(target -v);*/
/*      auto prevdiff = std::abs(target - closest_value);*/
/*      if (diff < prevdiff) {*/
/*        swappings.insert(p);*/
/*        backtrack(isBroken, pairs, loopAndGetZ, target, swappings, values, graph, v);*/
/*        swappings.erase(p);*/
/*        closest_value = v;*/
/*      }*/
/*      std::swap(std::get<1>(graph[p.first]), std::get<1>(graph[p.second]));*/
/*    }*/
/*  }*/
/*}*/
/*void backtrack_new(const std::vector<std::pair<int, int>>& pairs, int64_t bit_sim_from_target, std::set<std::pair<int, int>>& pairs_used,*/
/*std::vector<std::pair<std::tuple<std::string, std::string, std::string>, std::string>>& graph,*/
/*const std::map<std::string, int64_t>& values,*/
/*const std::function<int64_t(std::map<std::string, int64_t>)>& runLoopAndGetZ,*/
/*const std::function<int(int64_t)>& getBitSimilarity, int64_t z_num) {*/
/*  if (pairs_used.size() > 4) return;*/
/*  auto test = runLoopAndGetZ(values);*/
/*  std::cout << test << std::endl;*/
/*  if (test == z_num) {*/
/*      std::vector<std::string> outputs;*/
/*      for (const auto& p : pairs_used) {*/
/*        outputs.push_back(std::get<1>(graph[p.first]));*/
/*        outputs.push_back(std::get<1>(graph[p.second]));*/
/*      }*/
/*      std::sort(outputs.begin(), outputs.end());*/
/*      std::cout << join(outputs, ",") << std::endl;*/
/*      exit(0);*/
/*  }*/
/*  int64_t old_sim = bit_sim_from_target;*/
/*  for (int i = 0; i < pairs.size(); ++i) {*/
/*    auto found = std::find_if(pairs_used.begin(), pairs_used.end(), [&pairs, &i](const auto& pu) {*/
/*      return pu.first == pairs[i].first ||*/
/*              pu.first == pairs[i].second ||*/
/*              pu.second == pairs[i].first ||*/
/*              pu.second == pairs[i].second;*/
/*    });*/
/*    if (found != pairs_used.end()) continue;*/
/*    std::swap(std::get<1>(graph[pairs[i].first]), std::get<1>(graph[pairs[i].second]));*/
/*    auto v = runLoopAndGetZ(values);*/
/*    auto sim = getBitSimilarity(v);*/
/*    if (sim >= bit_sim_from_target) {*/
/*      bit_sim_from_target = sim;*/
/*    }*/
/*    std::swap(std::get<1>(graph[pairs[i].first]), std::get<1>(graph[pairs[i].second]));*/
/*  }*/
/*  assert(old_sim <= bit_sim_from_target);*/
/*  for (int i = 0; i < pairs.size(); ++i) {*/
/*    auto found = std::find_if(pairs_used.begin(), pairs_used.end(), [&pairs, &i](const auto& pu) {*/
/*      return pu.first == pairs[i].first ||*/
/*              pu.first == pairs[i].second ||*/
/*              pu.second == pairs[i].first ||*/
/*              pu.second == pairs[i].second;*/
/*    });*/
/*    if (found != pairs_used.end()) continue;*/
/*    std::swap(std::get<1>(graph[pairs[i].first]), std::get<1>(graph[pairs[i].second]));*/
/*    auto v = runLoopAndGetZ(values);*/
/*    auto sim = getBitSimilarity(v);*/
/*    if (sim == bit_sim_from_target) {*/
/*      pairs_used.insert(pairs[i]);*/
/*      backtrack_new(pairs, bit_sim_from_target, pairs_used, graph, values, runLoopAndGetZ, getBitSimilarity, z_num);*/
/*      pairs_used.erase(pairs[i]);*/
/*    }*/
/*    std::swap(std::get<1>(graph[pairs[i].first]), std::get<1>(graph[pairs[i].second]));*/
/*  }*/
/*}*/
