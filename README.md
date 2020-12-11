# Millet

Install dependencies by

    opam install menhir ocaml-vdom ocamlformat

and build Millet by running (requires OCaml >= 4.08.0)

    make

and you can clean up by running

    make clean

Millet gives you two options to run programs: 

- The first option is a web interface,
  accessible at `web/index.html`, which allows you to load one of the built-in
  examples or enter your own program, and then interactively click through all its
  (non-deterministic and asynchronous) reductions or introduce external interrupts.

- The second option is a command line executable run as

      ./cli.exe file1.mlt file2.mlt ...

  which loads all the commands in all the listed files and starts evaluating the
  given program, displaying all outgoing signals and the terminal configuration
  (if there is one). Non-deterministic reductions are chosen randomly and there is
  no option of introducing external interrupts. If you do not want to load the
  standard library, run Millet with the `--no-stdlib` option.
