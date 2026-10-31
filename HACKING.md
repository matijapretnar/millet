# Hacking on Millet

This document describes the architecture and implementation of Millet for anyone who wants to understand or modify the codebase. The architecture table in `README.md` lists the directory of each pipeline stage. The diagram below shows what each stage passes to the next:

```text
source text
    │  src/02-parser/lexer.mll + grammar.mly
    ▼
SugaredAst          (string names, every node location-annotated)
    │  src/03-desugarer/desugarer.ml
    ▼
Language.Ast        (Bindlib binders for bound variables, Symbol.t atoms for
    │                other names, no locations)
    │  src/04-typechecker/typechecker.ml
    ▼
Language.Ast        (unchanged; the bidirectional typechecker only checks)
    │  src/05-backends/interpreter/core/interpreter.ml
    ▼
small-step run_state (list of possible next reductions)
```

The `Loader` functor (`src/06-user-interface/core/loader.ml`) wires these stages together, parameterised over `Backend.S` (`src/05-backends/sig/core/backend.ml`).

## Current plans

Planned work, in order:

1. A REPL.
2. A module system.
3. A standard library, built on top of the module system.
4. A language server (LSP).
5. Rebase [AEff](https://github.com/matijapretnar/aeff) on top of Millet.
6. Rebase [Eff](https://github.com/matijapretnar/eff) on top of Millet.
7. A Jupyter kernel.

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
| local variable | `Var x` |
| top-level variable | `TopVar x` |
| constant (`42`, `"hi"`, `true`) | `Const c` |
| type annotation `(e : ty)` | `Annotated (e, ty)` |
| tuple `(e1, e2, e3)` | `Tuple [e1; e2; e3]` — empty tuple `()` is `Tuple []` |
| variant `Label e` | `Variant (lbl, Some e)` |
| `fun p -> t` | `Lambda (p, b)`, where `b` binds the variables of `p` in the body |
| `let rec f p -> t` | `RecLambda b'`, where `b'` binds `f` in the abstraction `(p, b)` |

**Computations** (may take reduction steps):

| Construct | Core AST |
| --- | --- |
| `return e` / any value | `Return e` |
| `let p = c1 in c2` | `Do (c1, (p, b))` |
| `f e` (application) | `Apply (f, e)` — both must be expressions |
| `match e with p -> c` | `Match (e, [(p, b); ...])` |

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

Patterns carry no names: the variables they bind are bound by the binder of the enclosing abstraction, in the order documented at `abstraction` in `ast.ml`.

| Construct | Core AST |
| --- | --- |
| variable `x` | `PVar` |
| type annotation `(p : ty)` | `PAnnotated (p, ty)` |
| alias `p as x` | `PAs p` |
| tuple `(p1, p2, p3)` | `PTuple [p1; p2; p3]` |
| variant `Label p` | `PVariant (lbl, Some p)` |
| constant `42` | `PConst c` |
| wildcard `_` | `PNonbinding` |

### Top-level commands

| Surface | Core AST | Effect |
| --- | --- | --- |
| `type t = ...` | `TyDef [...]` | registers a type definition |
| `let f (p1 : ty1) ... (pn : tyn) : ty = e` | `TopLet (f, (params, ty1 -> ... -> tyn -> ty), expr)` | binds a value with its declared type scheme in the global environment; `params` are the free type params of the declared type, and `n` may be 0 |
| `run t` | `TopDo comp` | enqueues a computation for evaluation |
| `let rec f (p1 : ty1) ... (pn : tyn) : ty = e` | `TopLet (f, (params, ty1 -> ... -> tyn -> ty), RecLambda ...)` | desugared to `TopLet` before reaching the core |

## Names and binding

The core AST uses two kinds of names, and the desugarer (`src/03-desugarer/desugarer.ml`) is the sole boundary between them and the string names of the surface syntax.

**Bound variables** are [Bindlib](https://github.com/rlepigre/ocaml-bindlib) variables: `variable = expression Bindlib.var`. Abstractions (`Lambda`, `Do`, `Match` cases) pair a pattern with a multiple binder over the body, and `RecLambda` binds the function in its abstraction. The desugarer builds terms as Bindlib boxes and returns unboxed commands. The interpreter substitutes values with `Bindlib.subst` and `Bindlib.msubst`, so there is no hand-written renaming or capture-avoiding substitution. The typechecker unbinds a body before checking it and keeps the types of local variables by Bindlib id.

**Atoms** are names that are never bound and that nothing is ever substituted for. Each kind is a separate module created with `Symbol.Make ()`:

```ocaml
module TyName = Symbol.Make ()   (* type constructor names *)
module TyParam = Symbol.Make ()  (* type parameters *)
module Label = Symbol.Make ()    (* variant constructor labels *)
module TopVar = Symbol.Make ()   (* top-level variables *)
```

A symbol is an `(int * string)` pair: a globally unique counter and a human-readable annotation used only for printing. Two symbols with the same annotation but different counters are distinct. Atoms are kept out of Bindlib on purpose: a Bindlib variable contains closures, so OCaml's structural equality fails on any value that contains one, whereas types, patterns and runtime values that contain only atoms can still be compared with `=`.

Top-level variables are atoms because they are constants with known bodies: the interpreter looks them up in its environment and the typechecker instantiates their declared schemes, while local variables are substituted and monomorphic.

## Type checking

`src/04-typechecker/typechecker.ml` implements a bidirectional typechecker: `infer_*` synthesises a type, `check_*` checks against an expected one, and the two meet at subsumption, where `unify` extends a substitution `ty TyParamMap.t ref`.

- Every top-level definition must declare its type: all its arguments and its result are annotated. The desugarer reads this header, quantifies over its free type params, and emits `TopLet (x, (params, ty), expr)`. The typechecker checks `expr` against `ty` and binds `x` to the declared scheme; it does not generalise.
- The quantified params are rigid: `unify` does not bind a param in `state.rigid_params`, so `let f (x : 'a) : 'a = x + 1` is rejected. Annotations in the body can refer to the params of the header, but not introduce new ones.
- All other `TyParam`s are unification variables, created by `fresh_ty`. Both kinds share the `TyParam` constructor.
- `TyTuple []` is the unit type — there is no separate unit constructor.
- Top-level variables have type schemes `(ty_param list * ty)` in `top_vars`, and `refreshing_subst` instantiates them with fresh unification variables at each use site. Local variables have monotypes in `local_vars`.

The following restrictions are deliberate. Do not lift them:

- Nothing is generalised. Every top-level definition and every recursive function declares its full type, and inner `let`s are monomorphic.
- There is no polymorphic recursion. It would be an easy extension, but type systems built on Millet may be incompatible with it.
- Unsolved unification variables are accepted. No behaviour depends on types, so they are harmless.

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

## Examples

The programs in `examples/` are run by the test suite and shown in the web interface. The web interface does not find them automatically: register each new example in `src/06-user-interface/web/dune` by adding its name and source to the rule that generates `examples_mlt.ml`.

## Tests

Tests live in `tests/` and are dune cram tests: each `.t` file holds shell commands together with their recorded output, and a test passes when the actual output matches the recorded output exactly.

- `tests/valid.t` runs `cli.exe` on every program in `tests/valid/` and `examples/`.
- `tests/invalid.t` runs `cli.exe` on every program in `tests/invalid/` and `tests/invalid/typechecker/`. These programs must be rejected, and the recorded output is the error message.

To run the tests:

```sh
make test
```

To add a test, create a new `.mlt` file in the appropriate directory. Then run `make test` once; it will fail and show the actual output as a diff. If the output is correct, promote it to become the new expected output with:

```sh
dune promote
```

Do not edit the `.t` files by hand.
