#!/usr/bin/env python3
"""Benchmark nabla's -Aa dispatch against the four individual algorithms.

Generates mutated DNA sequences from a real base genome at escalating edit
rates and, for each algorithm flag, measures wall-clock time and peak RSS
(with a timeout so the pathological cases are bounded rather than hang).

Usage:
  python3 benchmark.py [--length N] [--timeout S] [--base PATH]
                       [--trace] [--rates RATE ...]

--trace measures interval reconstruction (the full LCS); without it the
benchmark measures length-only (-l).  "trace" follows the code's traceLCS
gate (pairs != null), which is what enables dominant-match recording.
"""
import argparse
import collections
import os
import random
import re
import subprocess
import tempfile

NABLA = os.path.join(os.path.dirname(__file__), "..", "nabla", "build", "nabla")

FLAGS = [
  ("Myers (-Ad)", "-Ad"),
  ("Hunt-Szymanski (-Am)", "-Am"),
  ("RickSpace (-As)", "-As"),
  ("RickTime (-At)", "-At"),
  ("Auto (-Aa)", "-Aa"),
]
RATES = [0.01, 0.05, 0.10, 0.20, 0.40]


def load_sequence(path, n):
  with open(path) as f:
    return f.read().strip().upper()[:n]


def synthetic_base(length, alphabet_size, rng):
  """A random sequence of distinct code points, one per alphabet symbol."""
  return "".join(chr(0x100 + rng.randrange(alphabet_size)) for _ in range(length))


def mutate(seq, rate, rng):
  alphabet = sorted(set(seq)) or list("ACGT")
  out = []
  for ch in seq:
    if rng.random() < rate:
      op = rng.random()
      if op < 0.6:                 # substitution
        out.append(rng.choice([c for c in alphabet if c != ch]))
      elif op < 0.8:               # deletion
        continue
      else:                        # insertion
        out.append(rng.choice(alphabet))
        out.append(ch)
    else:
      out.append(ch)
  return "".join(out)


def match_count(seq1, seq2):
  """Total matched position pairs r = sum_s count1[s] * count2[s]."""
  c1 = collections.Counter(seq1)
  c2 = collections.Counter(seq2)
  return sum(c1[sym] * c2.get(sym, 0) for sym in c1)


def measure(flag, fa, fb, timeout, length_only):
  cmd = ["/usr/bin/time", "-v", NABLA, "-T", flag]
  if flag == "-Aa":
    cmd.append("-v")             # report which algorithm Automatic chose
  if length_only:
    cmd.append("-l")
  cmd.extend([fa, fb])
  try:
    # Full LCS writes the diff to stdout; discard it so we time the algorithm,
    # not terminal I/O.  /usr/bin/time -v reports wall time and peak RSS on
    # stderr, which we keep.
    p = subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE,
                       text=True, timeout=timeout)
    err = p.stderr
    rss = re.search(r"Maximum resident set size \(kbytes\): (\d+)", err)
    wall = re.search(r"Elapsed \(wall clock\) time \(h:mm:ss or m:ss\): ([\d:.]+)", err)
    rss = int(rss.group(1)) if rss else -1
    wall = wall.group(1) if wall else "-"
    choice = ""
    if flag == "-Aa":
      m = re.search(r"Automatic: selected (\S+) because", err)
      choice = m.group(1) if m else "?"
    return ("ok" if p.returncode == 0 else "abort", wall, rss, choice)
  except subprocess.TimeoutExpired:
    return ("TIMEOUT", f">{timeout}s", -1, "")


def main():
  ap = argparse.ArgumentParser()
  ap.add_argument("--length", type=int, default=50000)
  ap.add_argument("--timeout", type=int, default=20)
  ap.add_argument("--trace", action="store_true",
                  help="measure interval reconstruction (full LCS) instead of length-only")
  ap.add_argument("--rates", type=float, nargs="*", default=None)
  ap.add_argument("--base", default=os.path.join(
    os.path.dirname(__file__), "..", "data", "ecoli_mg1655.seq"))
  ap.add_argument("--alphabet", type=int, default=0,
                  help="generate a synthetic base over this many distinct symbols "
                       "(0 = use --base)")
  ap.add_argument("--seed", type=int, default=1)
  args = ap.parse_args()

  length_only = not args.trace
  rates = args.rates if args.rates is not None else RATES
  rng = random.Random(args.seed)
  if args.alphabet:
    base = synthetic_base(args.length, args.alphabet, rng)
    base_label = f"synthetic alphabet s={args.alphabet}"
  else:
    base = load_sequence(args.base, args.length)
    base_label = f"{args.base} (first {args.length} bases)"

  with tempfile.TemporaryDirectory() as d:
    fa = os.path.join(d, "base.seq")
    with open(fa, "w") as f:
      f.write(base)

    print(f"Base: {base_label}")
    print(f"Phase: {'length-only' if length_only else 'interval reconstruction (trace)'}")
    print(f"Timeout: {args.timeout}s per run\n")

    header = ["rate", "D", "r"] + [name for name, _ in FLAGS]
    print(" | ".join(h.ljust(20) for h in header))
    print("-" * (21 * (3 + len(FLAGS))))

    for rate in rates:
      mutated = mutate(base, rate, rng)
      fb = os.path.join(d, "mutated.seq")
      with open(fb, "w") as f:
        f.write(mutated)
      r = match_count(base, mutated)
      shortest = min(len(base), len(mutated))

      # Recover the LCS length p from a length-only -Aa run, so D = m+n-2p.
      cmd = [NABLA, "-T", "-Aa", "-l", fa, fb]
      try:
        out = subprocess.run(cmd, capture_output=True, text=True, timeout=args.timeout)
        m = re.search(r"LCS Length = ([\d,]+)", out.stdout + out.stderr)
        p = int(m.group(1).replace(",", "")) if m else -1
      except subprocess.TimeoutExpired:
        p = -1
      D = (len(base) + len(mutated) - 2 * p) if p >= 0 else -1

      cells = []
      for name, flag in FLAGS:
        status, wall, rss, choice = measure(flag, fa, fb, args.timeout, length_only)
        if status == "ok":
          cell = f"{wall} {rss // 1024}MB"
          if choice:
            cell += f" [{choice}]"
          cells.append(cell)
        else:
          cells.append(f"{status} ({wall})")
      print(" | ".join(c.ljust(20) for c in
            [f"{int(rate * 100)}%", str(D), str(r)] + cells), flush=True)

  print("\nEach cell is 'wall-time peak-RSS'; the Auto cell names the algorithm it chose.")
  print("TIMEOUT exceeded the limit; abort = budget bail-out.")
  print("Hunt-Szymanski is ineligible whenever the match count r >> 100 * min(m, n):")
  print("its O((r+n) log n) threshold list degenerates on small alphabets (DNA: r ~ n^2/4).")


if __name__ == "__main__":
  main()
