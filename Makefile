OCAMLMAKEFILE = OCamlMakefile

ANNOTATE = yes
export ANNOTATE

define PROJ_abnf
  SOURCES=abnf_syntaxtree.ml abnf_rules.ml abnf_ops.ml abnf_location.ml abnf_parser.mli abnf_lexer.ml abnf_parser.ml abnf_cmd.ml
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

ifndef SUBPROJS
  export SUBPROJS=abnf imapd
endif

all: dc
	@ :

%:
	@make -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
