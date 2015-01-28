
C = ocamlfind opt
LIBS = lablgtk2,xml-light,str
FILES = Main.cmx File.cmx Gui.cmx


%.cmx: %.ml
	$(C)  -c -package $(LIBS) $<

all: $(FILES)
	$(C) -o RUN.o -linkpkg -package $(LIBS)	$(FILES)

clean:
	rm -fv *.cmi *.cmx *.o