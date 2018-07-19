Modules := CommercialProperty 
VS := $(MODULES:%=%.v)
LIB := Realitz

.PHONY: coq clean 

coq : Makefile.coq
	$(MAKE) -f Makefile.coq

Makefile.coq: Makefile $(VS)
	coq_makefile -R . $(LIB) $(VS) -o Makefile.coq

clean:: Makefile.coq
	$(MAKE) -f Makefile.coq clean
	rm -f Makefile.coq
