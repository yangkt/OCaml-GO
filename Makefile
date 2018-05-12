test:
	ocamlbuild -use-ocamlfind board_test.byte && ./board_test.byte

text:
	ocamlbuild -use-ocamlfind text.byte && ./text.byte

clean:
	ocamlbuild -clean

zip:
	zip go.zip *.ml*
