#(********************************************************************)
#(* ocaml-scribble - Makefile                                        *)
#(********************************************************************)
#(* $Time-stamp: <Malo - 2012>$ *)


BINDIR := /usr/local/bin
LIBDIR := `ocamlc -where`
VERSION := `cat VERSION`


all: byte native
	cp oscribble.native oscribble

src/version.ml: VERSION
	echo 'let version ="' | cat - VERSION > src/version.ml
	echo '"' >> src/version.ml

native: src/version.ml
	ocamlbuild src/oscribble.native

byte: src/version.ml
	ocamlbuild src/oscribble.byte

clean: 
	ocamlbuild -clean
	rm -f src/version.ml
	rm -f *~
	rm -f oscribble
	rm -f src/*~
#	rm -f ex/test.tex

install: install-native

install-native:
	cp _build/src/oscribble.native $(BINDIR)/oscribble

install-byte:
	cp _build/src/oscribble.native $(BINDIR)/oscribble

test: all
	bash test/test.sh

dist: clean
	mkdir tmp
	cp -f AUTHORS CHANGELOG LICENSE  Makefile  NEWS  README TODO VERSION tmp/
	cp -r src/ tmp/
	cp -r ex/ tmp/
	mv tmp oscribble-$(VERSION)
	tar -cf oscribble-$(VERSION).tar oscribble-$(VERSION)
	gzip oscribble-$(VERSION).tar
	rm -rf oscribble-$(VERSION)