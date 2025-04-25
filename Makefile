# Makefile to tangle literate config files

EMACS=emacs
ORG_FILES=$(wildcard *.org)

all: tangle

tangle:
	@for file in $(ORG_FILES); do \
		echo "Tangling $$file..."; \
		$(EMACS) --batch -l org --eval "(progn \
		(find-file "$$file") \
		(org-babel-tangle))"; \
	done

clean:
	rm -f ~/.doom.d/init.el ~/.doom.d/config.el ~/.doom.d/packages.el ~/.doom.d/custom.el ~/.zshrc