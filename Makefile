# TARFILES = Makefile scanner.mll parser.mly ast.mli flio.ml

# OBJS = parser.cmo scanner.cmo flio.cmo
TARFILES = Makefile scanner.mll parser.mly ast.mli

OBJS = parser.cmo scanner.cmo

flio : $(OBJS)
	ocamlc -o flio $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

flio.tar.gz : $(TARFILES)
	cd .. && tar zcf flio/flio.tar.gz $(TARFILES:%=flio/%)

.PHONY : clean
clean :
	rm -f flio parser.ml parser.mli scanner.ml *.cmo *.cmi

# Generated by ocamldep *.ml *.mli
# flio.cmo: scanner.cmo parser.cmi ast.cmi 
# flio.cmx: scanner.cmx parser.cmx ast.cmi 
parser.cmo: ast.cmi parser.cmi 
parser.cmx: ast.cmi parser.cmi 
scanner.cmo: parser.cmi 
scanner.cmx: parser.cmx 
parser.cmi: ast.cmi 