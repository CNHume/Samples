// makedata.cpp — Generate LCS benchmark data from a FASTA (or plain) sequence.
//
// Usage:
//   makedata <input.fa> <output-prefix> <K1> [K2 ...]
//
// Reads <input.fa>, extracts the sequence (strips '>' header lines, drops
// whitespace, uppercases) and writes it as <output-prefix>.seq (one line).
// Then, for each K, applies K distinct random substitutions — each changed to
// a different symbol drawn from the sequence's own alphabet — and writes
// <output-prefix>_<K>subs.seq.  The xorshift32 generator is seeded, so a given
// input reproduces identical mutants on any platform.
//
// Build (Linux):   g++ -std=c++20 -O2 -Wall -Wextra makedata.cpp -o makedata
// Build (Windows): cl /std:c++20 /O2 /EHsc makedata.cpp
//
#include <algorithm>                    // for sort, unique
#include <cctype>                       // for isspace, toupper
#include <cstdint>                      // for uint32_t
#include <cstdio>                       // for fprintf
#include <cstdlib>                      // for exit, strtoull
#include <fstream>                      // for ifstream, ofstream
#include <string>                       // for string, to_string
#include <vector>                       // for vector

using namespace std;

static uint32_t rngState = 0x12345678u;
static uint32_t NextRand() {
  uint32_t x = rngState;
  x ^= x << 13;
  x ^= x >> 17;
  x ^= x << 5;
  return rngState = x;
}

//
// Extract the sequence from a FASTA (or plain) file: skip '>' header lines,
// drop whitespace, and uppercase.
//
static string ReadSequence(const string& path) {
  ifstream in(path, ios::binary);
  if (!in) {
    fprintf(stderr, "makedata: cannot open %s\n", path.c_str());
    exit(1);
  }
  string seq, line;
  while (getline(in, line)) {
    if (!line.empty() && line[0] == '>')
      continue;
    for (unsigned char c : line)
      if (!isspace(c))
        seq.push_back((char)toupper(c));
  }
  return seq;
}

static void WriteSequence(const string& path, const string& seq) {
  ofstream out(path, ios::binary);
  out << seq << '\n';
}

int main(int argc, char** argv) {
  if (argc < 4) {
    fprintf(stderr, "usage: makedata <input.fa> <output-prefix> <K1> [K2 ...]\n");
    return 2;
  }

  auto seq = ReadSequence(argv[1]);
  auto prefix = string(argv[2]);
  WriteSequence(prefix + ".seq", seq);

  // The sequence's own distinct symbols, used for substitutions.
  vector<char> symbols(seq.begin(), seq.end());
  sort(symbols.begin(), symbols.end());
  symbols.erase(unique(symbols.begin(), symbols.end()), symbols.end());

  auto n = seq.size();
  vector<size_t> pos(n);
  for (size_t i = 0; i < n; i++)
    pos[i] = i;

  for (int a = 3; a < argc; a++) {
    auto K = (size_t)strtoull(argv[a], nullptr, 10);
    if (K > n)
      K = n;

    // Fisher-Yates shuffle to select K distinct positions.
    for (size_t i = n - 1; i > 0; i--) {
      auto r = NextRand() % (i + 1);
      swap(pos[i], pos[r]);
    }

    auto mut = seq;
    for (size_t k = 0; k < K; k++) {
      auto p = pos[k];
      char c;
      do {
        c = symbols[NextRand() % symbols.size()];
      } while (c == mut[p]);
      mut[p] = c;
    }
    WriteSequence(prefix + "_" + to_string(K) + "subs.seq", mut);
  }
  return 0;
}
