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
