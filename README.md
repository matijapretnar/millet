# Millet

Do you, like me, test theoretical programming language concepts by building your own programming language? Do you, like me, do it by copying and modifying your most recent language because you are too lazy to build everything from scratch? Do you, like me, end up with a mess?

To explore asynchronous effects with [AEff](https://github.com/matijapretnar/aeff), I took [Eff](https://github.com/matijapretnar/eff), dropped the handler-related code, and added asynchronous operations and promises. In the process I spotted many things I wanted to improve and bring back to Eff as well. As every computer scientist knows, the only possible numbers of occurrences are zero, one, and infinity, and I saw into which category I fell. So I extracted all the boring but nice-to-have bits (typechecker, datatypes, pattern matching, standard library, …), cleaned it up, and turned it into Millet: a modular template that future languages can be built on top of, pulling in upstream improvements as they come.

## How to install and run Millet?

Install dependencies with

    opam install menhir vdom ocamlformat=0.27.0

then build (requires OCaml >= 4.14.0) with

    make

and clean up with

    make clean

Millet programs are files with the `.mlt` extension. Run them with the command-line interface:

    ./cli.exe file1.mlt file2.mlt ...

which loads the standard library and all listed files in order, then evaluates any `run` expressions. To skip the standard library:

    ./cli.exe --no-stdlib file.mlt

A web interface is also available at `web/index.html` (or online at <https://matija.pretnar.info/millet/>), where you can load built-in examples or enter your own program and step through reductions interactively.

## What does Millet look like?

Millet is a pure functional language in the ML family. It has:

- **Algebraic data types** — `type`, sum types with `|`, pattern matching with `match`/`with`
- **Polymorphic type inference** — Hindley-Milner, no annotations required
- **Recursive functions** — `let rec`
- **Lists** — built-in with `[]`, `::`, and pattern matching
- **Tuples** — `(a, b, c)`
- **The usual arithmetic and comparison operators**

A small example:

    type 'a tree =
      | Leaf
      | Node of 'a tree * 'a * 'a tree

    let rec insert x = function
      | Leaf -> Node (Leaf, x, Leaf)
      | Node (l, y, r) ->
        if x < y then Node (insert x l, y, r)
        else if x > y then Node (l, y, insert x r)
        else Node (l, y, r)

    let rec to_list = function
      | Leaf -> []
      | Node (l, x, r) -> to_list l @ [x] @ to_list r

    run to_list (insert 3 (insert 1 (insert 2 Leaf)))

The `run` keyword evaluates a top-level expression and prints the result.

## How is Millet structured?

The codebase is a pipeline of independent stages, each in its own numbered directory:

Stage          | Directory                      | Description
-------------- | ------------------------------ | -----------
Language       | `src/01-language/`             | Core AST, constants, and primitive operations
Parser         | `src/02-parser/`               | Source text → sugared AST (string names, locations)
Desugarer      | `src/03-desugarer/`            | Sugared AST → core AST (symbolic names, no locations)
Typechecker    | `src/04-typechecker/`          | Hindley-Milner inference and checking
Backends       | `src/05-backends/`             | Execution backends (small-step interpreter, compilation, …)
User interface | `src/06-user-interface/`       | CLI and web drivers, standard library
Utilities      | `src/00-utils/`                | Shared utilities (errors, locations, symbols, printing)

See `HACKING.md` for a detailed description of the architecture and implementation.

## How to use Millet as a template?

Millet is designed to be forked and modified. The intended workflow uses two branches: `main` for your own language and `millet` for tracking upstream so you can pull in future improvements cleanly.

Create a new repository and add Millet as a remote:

    mkdir the-best-language && cd the-best-language
    git init
    git remote add millet git@github.com:matijapretnar/millet.git
    git fetch millet

Create two branches:

    git branch --no-track main millet/main
    git branch --track millet millet/main
    git checkout main

When Millet updates, merge the changes in:

    git checkout millet && git pull
    git checkout main && git merge millet

## Why the name Millet?

Millet uses a fine-grain call-by-value core calculus, and there is no finer grain than [millet](https://en.wikipedia.org/wiki/Millet). Plus, the `.mlt` extension fits nicely into the ML family.
