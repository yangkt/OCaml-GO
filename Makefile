test:
	ocamlbuild -use-ocamlfind board_test.byte && ./board_test.byte

clean:
	ocamlbuild -clean

zip:
	zip go.zip *.ml*
