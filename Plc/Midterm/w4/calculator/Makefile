FLAGS = -use-menhir -I src
CMD   = ocamlbuild $(FLAGS) 

main = native

native:
	$(CMD) main.ml main.native

clean:
	$(CMD) -clean

sanity:
	which menhir




