# Millet — LLM context

Millet is a pure ML-like functional language and template for building new
languages. See `README.md` for usage and history, `HACKING.md` for the
pipeline, architecture, implementation details, deliberate restrictions,
tests, and current plans.

## Rules that are easy to break

- Do not lift the deliberate restrictions listed under "Type checking" in
  `HACKING.md`.
- Do not use Bindlib variables for atoms. See "Names and binding" in
  `HACKING.md`.
- Do not edit the `.t` files by hand. Run `dune promote` to accept a new
  snapshot.
- Register each new example in `src/06-user-interface/web/dune`.
- Do not overexplain functional concepts in comments or examples (e.g. how fold works, what tail recursion is) — the target audience is functional language implementors, not students.

## Build

Build and test in the `millet` opam switch, e.g.
`opam exec --switch=millet -- dune test`. Bindlib must be installed there.
