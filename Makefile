.PHONY: build test opam release distrib extract prove melocoton

build:
	dune build --root . @install

test:
	dune runtest --root .

opam:
	dune build --root . *.opam

# make release TAG=x.y, from an up-to-date clean main whose CHANGES starts
# with "## x.y ##"
release:
	@test -n "$(TAG)" || { echo "usage: make release TAG=x.y"; exit 1; }
	@test "$$(sed -n '1s/^## *\([^ ]*\).*/\1/p' CHANGES)" = "$(TAG)" \
	  || { echo "CHANGES does not start with ## $(TAG) ##"; exit 1; }
	@git diff --quiet HEAD || { echo "uncommitted changes"; exit 1; }
	@git fetch -q origin main
	@test "$$(git rev-parse HEAD)" = "$$(git rev-parse origin/main)" \
	  || { echo "HEAD is not origin/main"; exit 1; }
	@sed -n '1p;2,/^##/{/^##/!p;}' CHANGES
	@echo "Is the CHANGES correct for $(TAG) (y/n)?"
	@read yesno; test "$$yesno" = y
	dune-release tag $(TAG)
	$(MAKE) distrib TAG=$(TAG)
	dune-release publish
	dune-release opam pkg
	dune-release opam submit

# The release archive _build/camlid-$(TAG).tbz of the tag $(TAG)
distrib:
	@test -n "$(TAG)" || { echo "usage: make distrib TAG=x.y"; exit 1; }
	dune-release distrib
	@# dune-release keeps every file but .git: repack without the agents'
	@# files (not tar --delete: it corrupted the archive with GNU tar on
	@# Alpine), then check that the archive is git archive of the tag
	@# (export-ignore) up to the version added by dune subst
	d=_build/distrib-$$$$ && mkdir -p $$d/a $$d/b $$d/c \
	  && bzip2 -dc _build/camlid-$(TAG).tbz | tar x -C $$d/a \
	  && tar c -C $$d/a --format=ustar --sort=name --owner=0 --group=0 \
	       --numeric-owner --anchored --exclude=camlid-$(TAG)/AGENTS.md \
	       --exclude=camlid-$(TAG)/CLAUDE.md --exclude=camlid-$(TAG)/agents \
	       camlid-$(TAG) | bzip2 > _build/camlid-$(TAG).tbz \
	  && bzip2 -dc _build/camlid-$(TAG).tbz | tar x -C $$d/b \
	  && git archive --prefix=camlid-$(TAG)/ $(TAG) | tar x -C $$d/c \
	  && cp $$d/b/camlid-$(TAG)/camlid.opam $$d/b/camlid-$(TAG)/dune-project \
	       $$d/c/camlid-$(TAG)/ \
	  && diff -r $$d/b $$d/c

# Extract the proved scheduler (needs why3, see agents/skills/why3.md)
extract:
	cd proof && why3 extract --warn-off=clone_not_abstract -D ocaml64 -D camlid.drv -L . \
	  plan.Types plan.Lists schedule.Schedule -o ../lib/plan_gen.ml.tmp
	(echo "(* Extracted from proof/schedule.mlw by make extract: do not edit *)"; \
	 cat lib/plan_gen.ml.tmp) > lib/plan_gen.ml
	rm lib/plan_gen.ml.tmp

prove:
	cd proof && why3find prove -j 4 plan.mlw spec.mlw schedule.mlw memory.mlw

# Check two generated stubs in Melocoton (see proof/melocoton/README.md)
MELOCOTON ?= $(HOME)/melocoton
MELOCOTON_SWITCH ?= melocoton
melocoton:
	opam exec --switch=$(MELOCOTON_SWITCH) -- coqc \
	  -R $(MELOCOTON)/_build/default/theories melocoton \
	  -w -notation-overridden -w -redundant-canonical-projection \
	  -w -inconsistent-scopes proof/melocoton/camlid_custom.v
