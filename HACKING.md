# Hacking on Millet

This document describes the architecture and implementation of Millet for anyone who wants to understand or modify the codebase. For a high-level overview of the pipeline stages, see the architecture table in `README.md`.

## Current plans

The main goal is to rebuild [Eff](https://github.com/matijapretnar/eff) on top of Millet, adding algebraic effects and handlers as a language extension rather than a separate codebase. Alongside that: a REPL, an extended standard library, a module system, more examples, and eventually a language server (LSP).

## The expression/computation split

The core language (`src/01-language/ast.ml`) distinguishes two syntactic categories:

- **Expressions** — pure values: variables, constants, tuples, variants, lambdas. An expression never has a side effect and always reduces to itself.
- **Computations** — things that "do something": `Return`, `Do`, `Match`, `Apply`. A computation may take steps.

This is *fine-grain call-by-value*: every argument to a function must be an expression (a value), never a computation. The desugarer enforces this by hoisting any computation-valued sub-expression into a `Do` binding. For example, the expression `(1 + 2) * (3 + 4)` desugars to:

   let x = 1 + 2 in
   let y = 3 + 4 in
   x * y

The expression/computation split simplifies the semantics and makes it easier to add effects later. The interpreter's small-step semantics only needs to define reduction rules for computations, and the typechecker can assign types to expressions and computations separately.

## AST

The core AST lives in `src/01-language/ast.ml`.

### Expressions and computations

Everything in the surface language desugars to a small set of core constructs.

**Expressions** (pure values, never take a step):

| Construct | Core AST |
| --- | --- |
| variable | `Var x` |
| constant (`42`, `"hi"`, `true`) | `Const c` |
| type annotation `(e : ty)` | `Annotated (e, ty)` |
| tuple `(e1, e2, e3)` | `Tuple [e1; e2; e3]` — empty tuple `()` is `Tuple []` |
| variant `Label e` | `Variant (lbl, Some e)` |
| `fun p -> t` | `Lambda (p, comp)` |
| `let rec f p -> t` | `RecLambda (f, (p, comp))` |

**Computations** (may take reduction steps):

| Construct | Core AST |
| --- | --- |
| `return e` / any value | `Return e` |
| `let p = c1 in c2` | `Do (c1, (p, c2))` |
| `f e` (application) | `Apply (f, e)` — both must be expressions |
| `match e with p -> c` | `Match (e, [(p, c); ...])` |

There are additional surface constructs that desugar away entirely:

| Surface | Desugars to |
| --- | --- |
| `if e then t1 else t2` | `Match` on `true`/`false` constants |
| `e1 && e2`, `e1 \|\| e2` | short-circuit `Match` (not primitives) |
| `function p -> t \| ...` | `Lambda` wrapping a `Match` |
| `[]`, `x :: xs` | `Variant` with `nil_label` / `cons_label` |

### Types

The `ty` type represents Millet types:

| Constructor | Meaning |
| --- | --- |
| `TyConst c` | primitive constant type (`int`, `bool`, `string`, `float`) |
| `TyApply (name, args)` | named type application, e.g. `'a list` |
| `TyParam a` | type variable `'a` |
| `TyArrow (t1, t2)` | function type `t1 -> t2` |
| `TyTuple tys` | product type `t1 * t2 * ...` |

`TyConst` holds the four primitive constant types. They are also registered in the typechecker's initial state as inline type definitions (`TyInline (TyConst ...)`) so that the user-facing names `int`, `bool`, `string`, and `float` resolve to them via `TyApply`. Inside the typechecker `TyConst` can therefore appear wherever a concrete type is needed, but user code never writes it directly.

### Patterns

| Construct | Core AST |
| --- | --- |
| variable `x` | `PVar x` |
| type annotation `(p : ty)` | `PAnnotated (p, ty)` |
| alias `p as x` | `PAs (p, x)` |
| tuple `(p1, p2, p3)` | `PTuple [p1; p2; p3]` |
| variant `Label p` | `PVariant (lbl, Some p)` |
| constant `42` | `PConst c` |
| wildcard `_` | `PNonbinding` |

### Top-level commands

| Surface | Core AST | Effect |
| --- | --- | --- |
| `type t = ...` | `TyDef [...]` | registers a type definition |
| `let x = e` | `TopLet (x, expr)` | binds a value in the global environment |
| `run t` | `TopDo comp` | enqueues a computation for evaluation |
| `let rec f = e` | `TopLet (f, RecLambda ...)` | desugared to `TopLet` before reaching the core |

## The Symbol system

> **Planned change:** `Symbol` will be replaced by [bindlib](https://github.com/rlepigre/ocaml-bindlib), a library that handles variable binding and substitution generically. The manual refresh/substitution machinery in the interpreter will go away.

All names in the core AST are **typed symbols** rather than strings. Each symbol module is created independently with `Symbol.Make ()`:

```ocaml
module Variable = Symbol.Make ()   (* term-level variables *)
module TyName   = Symbol.Make ()   (* type constructor names *)
module TyParam  = Symbol.Make ()   (* type parameters *)
module Label    = Symbol.Make ()   (* variant constructor labels *)
```

A symbol is an `(int * string)` pair: a globally unique counter and a human-readable annotation used only for printing. Two symbols with the same annotation but different counters are distinct — there are no accidental name collisions.

The **desugarer** (`src/03-desugarer/desugarer.ml`) is the sole boundary between string-based and symbol-based names. It maintains `StringMap`-based scope tables and converts every string name to a fresh symbol. After desugaring, no string lookups exist in the pipeline.

`Variable.refresh x` creates a fresh symbol with the same annotation as `x`. The interpreter uses this to rename bound variables when substituting into recursive closures, preventing variable capture.

## Type inference

`src/04-typechecker/typechecker.ml` implements Hindley-Milner inference as constraint generation followed by unification.

> **Planned change:** The HM typechecker will be replaced with a bidirectional typechecker, which is easier to extend and provides better error messages.

- `infer_expression` and `infer_computation` each return `(ty * (ty * ty) list)`: the inferred type plus a list of equality constraints accumulated along the way.
- `unify` solves the constraint list and returns a substitution `ty TyParamMap.t`.
- `TyTuple []` is the unit type — there is no separate unit constructor.
- Type schemes `(ty_param list * ty)` are stored in the variable environment. `refreshing_subst` instantiates them with fresh type variables at each use site, giving let-polymorphism.

## The small-step interpreter

`src/05-backends/interpreter/core/interpreter.ml` implements a *small-step* operational semantics. `step_computation` returns a list of possible next reductions, each paired with a label and thunked. An empty list means the computation is a final value (`Return`). Multiple entries mean several reductions are enabled simultaneously.

The runtime layer picks one step to take: the CLI picks randomly, the web UI lets the user choose. This design makes it straightforward to extend the template with non-deterministic or concurrent semantics — the drivers already handle multiple enabled steps.

## The Loader

`src/06-user-interface/core/loader.ml` defines a `Loader` functor parameterised over `Backend.S`. It wires the pipeline together, threading state through the desugarer, typechecker, and backend in lockstep. Swapping in a new backend (e.g. a compiler) requires only a new `Backend.S` module and a new driver — the loader, typechecker, and desugarer are unchanged.

Primitives are loaded at startup through `load_primitive`, which registers each primitive in all three layers simultaneously.

The standard library (`src/06-user-interface/core/stdlib.mlt`) is embedded into the binary at build time: a dune rule generates a `Stdlib_mlt` OCaml module whose `contents` field holds the file as a string. The loader reads this string just like any other source file.

## How to add a primitive

Primitives are built-in operations implemented in OCaml and exposed to user code via `__double_underscore__` names. Adding one touches four files:

1. **`src/01-language/primitives.ml`** — add a variant to `type primitive`, add it to the `primitives` list (this list drives loading; omitting it means the primitive is silently never registered), and add a case to `primitive_name` returning its `__name__` string.
2. **`src/04-typechecker/primitives.ml`** — add a case to `primitive_type_scheme` returning the primitive's type scheme.
3. **`src/05-backends/interpreter/core/primitives.ml`** — add a case to `primitive_function` returning the OCaml function that implements it. The function takes an `Ast.expression` and returns an `Ast.computation`.
4. **`src/06-user-interface/core/stdlib.mlt`** — expose it under a user-facing name, e.g. `let print x = __print__ x`.

## How to add syntax

New surface syntax requires changes to the parser and desugarer:

1. **`src/02-parser/sugaredAst.ml`** — add a new variant to `plain_term` (or `plain_command` for top-level constructs).
2. **`src/02-parser/grammar.mly`** — add the token(s) to the lexer section if needed, and add grammar rules that produce the new `SugaredAst` variant.
3. **`src/02-parser/lexer.mll`** — add any new keywords or tokens.
4. **`src/03-desugarer/desugarer.ml`** — add a case to `desugar_plain_computation` (or `desugar_plain_expression` / `desugar_command`) that translates the new surface construct into core AST.

If the new construct has a core-language counterpart, also extend `src/01-language/ast.ml` and all the functions that pattern-match on `computation` or `expression` throughout the typechecker and interpreter.

## How to add a backend

The `Backend.S` module type is defined in `src/05-backends/sig/core/backend.ml`. Implement this signature, then instantiate the `Loader` functor with your new backend module and write a driver (following `src/06-user-interface/cli/cli.ml`).

## Tests

Tests live in `tests/`. The entire suite is driven by a single shell script in `tests/run_tests.t`: it runs `cli.exe` on every `.mlt` file in the directory and records the output. The recorded output is the expected output — the test passes when the actual output matches it exactly.

To run the tests:

```sh
make test
```

To add a test, create a new `.mlt` file in `tests/`. Then run `make test` once; it will fail and show the actual output as a diff. Promote the output to become the new expected output with:

```sh
dune promote
```
