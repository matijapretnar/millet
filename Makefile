default: format
	dune build

format:
	dune build @fmt --auto-promote

release: format
	dune build --profile release

test: default
	dune test

clean:
	dune clean

.PHONY: default format release test clean
