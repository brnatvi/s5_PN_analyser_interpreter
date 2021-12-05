binary:
	eval `opam env` && dune build polish.exe

byte:
	dune build polish.bc

clean:
	dune clean
