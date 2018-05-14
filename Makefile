
test:
	ocamlbuild -use-ocamlfind go_test.byte && ./go_test.byte

text:
	ocamlbuild -use-ocamlfind text.byte && ./text.byte

clean:
	ocamlbuild -clean

zip:
	zip go.zip *.ml*
