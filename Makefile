default: format
	dune build

format:
	dune build @fmt --auto-promote

release: format
	dune build --profile release

clean:
	dune clean

.PHONY: default format release clean
