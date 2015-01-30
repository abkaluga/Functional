
C = ocamlfind opt
LIBS = lablgtk2,xml-light,str


sms.cmx: sms.ml
	$(C)  -c -package $(LIBS) sms.ml

File.cmx:  File.ml sms.cmx
		$(C)  -c  sms.cmx -package $(LIBS) File.ml

Gui.cmx: Gui.ml sms.cmx File.cmx	
		$(C)  -c sms.cmx -package  File.cmx $(LIBS) Gui.ml

run: sms.cmx File.cmx Gui.cmx
	$(C) -o RUN  -linkpkg -package $(LIBS)	

clean:
	rm -fv *.cmi *.cmx *.o