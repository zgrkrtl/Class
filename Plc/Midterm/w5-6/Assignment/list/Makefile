FLAGS = -use-menhir -I src 
CMD   = ocamlbuild $(FLAGS)

all: native 

clean:
	$(CMD) -clean

native:
	$(CMD) main.native

sanity:
	which menhir
