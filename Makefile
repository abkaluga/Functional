
C = ocamlfind opt
LIBS = lablgtk2,xml-light,str

run: o.ml
	$(C) -o RUN.out  -linkpkg -package $(LIBS)	o.ml

clean:
	rm -fv *.cmi *.cmx *.o *.out