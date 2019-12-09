MODULES=query datardwt computation main authors
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)'
PKGS=oUnit,str,ANSITerminal

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

bisect-test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST) -runner sequential

bisect: clean bisect-test
	bisect-ppx-report -I _build -html report bisect0001.out

query:
	$(OCAMLBUILD)  $(MAIN) && rlwrap ./$(MAIN)

sad-query:
	$(OCAMLBUILD)  $(MAIN) && ./$(MAIN)

zip:
	zip dbms.zip *.ml* _tags Makefile
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report search_src.zip bisect*.out
