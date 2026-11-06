# Contributing

Working principles for the `nabla` LCS project under `C++/LCS/`.  Their goal
is to make a change cheap to review and correct *before* it fans out across
many files.

## The contract-first rule

Settle the contract while it is still cheap to change, then apply it.

1. **Agree on definitions first, then apply them.**  When a term, symbol, or
   convention is adopted, agree on what it *denotes* — not just the word.
   A bare label cannot be shown wrong until it has been applied everywhere;
   a `term = definition` mapping can be, up front.
2. **Settle the design first, then implement.**  When code is about to change,
   agree on the design — what changes, what does not, and the invariant being
   preserved — before writing it.

These are one rule at two levels: a definition is the contract for
terminology; a design is the contract for code.  Both are trivial to revise
in a sentence and expensive once propagated.

## The artifacts

Before a multi-file change, hand back a short contract for a single approval,
then apply it.

- **Terminology** — a notation block:

  ```
  `r` = match count;  density `ρ = r/(mn)` ∈ [0,1];  dense/sparse = adjectives.
  ```

- **Design** — a short statement:

  ```
  Rename `SparseRedundancy` → `SparseMatchCount`; no behavior change;
  update the two dispatch call sites and the docs that name it.
  ```

The artifact should be *small and reviewable*: its purpose is to be approved
or corrected in one glance, not to be exhaustive.

## Why this is written down

These rules are recorded after a concrete miss.  "Density" was applied
repo-wide as the term for the raw match count `r`, only for it to emerge that
density conventionally denotes a normalized ratio; the correct mapping is
`r` = match count and density = `r/(mn)`.  The entire sweep had to be undone
and redone.  A single `term = definition` confirmation beforehand would have
caught it, and the second pass — which stated the mapping up front — produced
no further rework.
