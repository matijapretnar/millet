# Millet

Do you, like me, test theoretical programming language concepts by building your own programming language? Do you, like me, do it by copying and modifying your most recent language because you are too lazy to build everything from scratch? Do you, like me, end up with a mess? Then Millet is for you. It is a pure ML-like language with simple and modular codebase that you can use as a template for your next language.

## How to install and run Millet?

Install dependencies by

    opam install menhir vdom ocamlformat

and build Millet by running (requires OCaml >= 4.14.0)

    make

and you can clean up by running

    make clean

Millet gives you two options to run programs:

- The first option is a web interface, accessible at `web/index.html`, which allows you to load one of the built-in examples or enter your own program, and then interactively click through all its (non-deterministic and asynchronous) reductions or introduce external interrupts. The web interface is also available at <https://matija.pretnar.info/millet/>.

- The second option is a command line executable run as

      ./cli.exe file1.mlt file2.mlt ...

  which loads all the commands in all the listed files and starts evaluating the given program, displaying all outgoing signals and the terminal configuration (if there is one). Non-deterministic reductions are chosen randomly and there is no option of introducing external interrupts. If you do not want to load the standard library, run Millet with the `--no-stdlib` option.

## How to use Millet as a template?

The easiest way is to first create an empty repository:

    mkdir the-best-language
    cd the-best-language
    git init

Next, add Millet's remote repository:

    git remote add millet git@github.com:matijapretnar/millet.git
    git fetch millet

Then, create two branches, one for your main development and one for tracking Millet:

    git branch --no-track main millet/main
    git branch --track millet millet/main
    git checkout main

Now, each time Millet updates, you can run

    git checkout millet
    git pull
    git checkout main
    git merge millet

to pull latest changes and merge them into your main development.

## Why the name Millet?

Millet uses fine-grain call-by-value core calculus, and there is no finer grain than [millet](https://en.wikipedia.org/wiki/Millet). Plus, the `.mlt` extension fits nicely into the ML family.
