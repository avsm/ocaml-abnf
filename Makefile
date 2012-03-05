OCAMLMAKEFILE = OCamlMakefile

ANNOTATE = yes
export ANNOTATE
LLVM ?= /opt/local

define PROJ_abnf
  SOURCES=abnf_syntaxtree.ml abnf_rules.ml abnf_ops.ml abnf_location.ml abnf_parser.mli abnf_lexer.ml abnf_parser.ml abnf_recursive_descent.ml abnf_signature.ml abnf_cmd.ml
  RESULT=abnf_cmd
  LIBS=str
  TRASH=abnf_lexer.ml abnf_parser.ml abnf_lexer.mli abnf_parser.mli
endef
export PROJ_abnf

define PROJ_imapd
  SOURCES=imap_parser.ml imap_terms.ml imap_rules.ml imapd.ml
  RESULT=imapd
endef
export PROJ_imapd

define PROJ_llvm
  SOURCES=llvmtest.ml
  RESULT=llvmtest
  INCDIRS=$(LLVM)/lib/ocaml
  LIBS=llvm llvm_bitwriter
  CLIBS=stdc++ llvm
endef
export PROJ_llvm
 
ifndef SUBPROJS
  export SUBPROJS=abnf imapd
endif

all: dc
	@ :

lltest:
	./llvmtest foo.bc
	$(LLVM)/bin/llvm-dis foo.bc > foo.ll
	$(LLVM)/bin/llvmc foo.ll -o foo
	./foo; echo $$?
	cat foo.ll

%:
	@make -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
