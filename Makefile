build:
	dune build --root . @install

test:
	dune runtest --root .

opam:
	dune build --root . *.opam

DESCRIPTION="$(shell sed -n -e "p;n;:next;/^##/Q;p;n;b next" CHANGES | perl -pe 's/\n/\\n/')"

release:
	@echo -n $(DESCRIPTION)
	@echo "Is the CHANGES correct for $(TAG) (y/n)?"
	@read yesno; test "$$yesno" = y
	dune-release tag $(TAG)
	dune-release distrib --skip-lint
	dune-release publish
	dune-release opam pkg
	dune-release opam submit

# Extract the proved scheduler (needs why3, see agents/skills/why3.md)
extract:
	cd proof && why3 extract -D ocaml64 -D camlid.drv -L . \
	  plan.Types plan.Lists schedule.Schedule -o ../lib/plan_gen.ml.tmp
	(echo "(* Extracted from proof/schedule.mlw by make extract: do not edit *)"; \
	 cat lib/plan_gen.ml.tmp) > lib/plan_gen.ml
	rm lib/plan_gen.ml.tmp

prove:
	cd proof && why3find prove -j 4 plan.mlw spec.mlw schedule.mlw blocks.mlw

# Check two generated stubs in Melocoton (see proof/melocoton/README.md)
MELOCOTON ?= $(HOME)/melocoton
MELOCOTON_SWITCH ?= melocoton
melocoton:
	opam exec --switch=$(MELOCOTON_SWITCH) -- coqc \
	  -R $(MELOCOTON)/_build/default/theories melocoton \
	  -w -notation-overridden -w -redundant-canonical-projection \
	  -w -inconsistent-scopes proof/melocoton/camlid_custom.v
