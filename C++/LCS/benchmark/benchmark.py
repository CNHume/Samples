#!/usr/bin/env python3
"""Benchmark nabla's -Aa dispatch against the four individual algorithms.

Generates mutated DNA sequences from a real base genome at escalating edit
rates and, for each algorithm flag, measures wall-clock time and peak RSS
(with a timeout so the pathological cases are bounded rather than hang).

Usage:
  python3 benchmark.py [--length N] [--timeout S] [--base PATH]
"""
import argparse
import random
import re
import subprocess
import tempfile
import os

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


def measure(flag, fa, fb, timeout):
  # -l = length only: isolates the algorithm's find phase from formatting.
  cmd = ["/usr/bin/time", "-v", NABLA, "-T", flag, "-l", fa, fb]
  try:
    p = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)
    err = p.stderr
    rss = re.search(r"Maximum resident set size \(kbytes\): (\d+)", err)
    wall = re.search(r"Elapsed \(wall clock\) time \(h:mm:ss or m:ss\): ([\d:.]+)", err)
    rss = int(rss.group(1)) if rss else -1
    wall = wall.group(1) if wall else "-"
    length = p.stdout.strip()
    return ("ok" if p.returncode == 0 else f"rc={p.returncode}", wall, rss, length)
  except subprocess.TimeoutExpired:
    return ("TIMEOUT", f">{timeout}s", -1, "-")


def main():
  ap = argparse.ArgumentParser()
  ap.add_argument("--length", type=int, default=50000)
  ap.add_argument("--timeout", type=int, default=20)
  ap.add_argument("--base", default=os.path.join(
    os.path.dirname(__file__), "..", "data", "ecoli_mg1655.seq"))
  ap.add_argument("--seed", type=int, default=1)
  args = ap.parse_args()

  rng = random.Random(args.seed)
  base = load_sequence(args.base, args.length)

  with tempfile.TemporaryDirectory() as d:
    fa = os.path.join(d, "base.seq")
    with open(fa, "w") as f:
      f.write(base)

    print(f"Base: {args.base} (first {args.length} bases)")
    print(f"Flags: {[f for _, f in FLAGS]}")
    print(f"Timeout: {args.timeout}s per run\n")

    header = ["rate", "D"] + [name for name, _ in FLAGS]
    print(" | ".join(h.ljust(22) for h in header))
    print("-" * (24 + 24 * len(FLAGS)))

    for rate in RATES:
      mutated = mutate(base, rate, rng)
      fb = os.path.join(d, "mutated.seq")
      with open(fb, "w") as f:
        f.write(mutated)

      # Run -Aa once, unmeasured, to recover the LCS length p (so D = m+n-2p).
      cmd = [NABLA, "-T", "-Aa", "-l", fa, fb]
      try:
        r = subprocess.run(cmd, capture_output=True, text=True, timeout=args.timeout)
        m = re.search(r"LCS Length = ([\d,]+)", r.stdout + r.stderr)
        p = int(m.group(1).replace(",", "")) if m else -1
      except subprocess.TimeoutExpired:
        p = -1
      D = (len(base) + len(mutated) - 2 * p) if p >= 0 else -1

      cells = []
      for name, flag in FLAGS:
        status, wall, rss, _ = measure(flag, fa, fb, args.timeout)
        if status == "ok":
          cells.append(f"{wall}  {rss//1024}MB")
        else:
          cells.append(f"{status} ({wall})")
      print(" | ".join(c.ljust(22) for c in
            [f"{int(rate*100)}%", str(D)] + cells), flush=True)

  print("\nEach cell is 'wall-time  peak-RSS'; TIMEOUT means it exceeded the limit.")


if __name__ == "__main__":
  main()
