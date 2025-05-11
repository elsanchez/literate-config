# Makefile to tangle literate config files for Doom Emacs and Zsh

EMACS=emacs
ORG_FILES=$(wildcard *.org)

all: tangle

tangle:
	mkdir -p ~/.config/doom/lisp
	@for file in $(ORG_FILES); do \
		echo "Tangling $$file..."; \
		$(EMACS) --batch -l org --eval "(progn \
			(find-file \"$$file\") \
			(org-babel-tangle) \
			(save-some-buffers t))"; \
	done
	@echo "✅ Tangling completed!"

scripts:
	@echo "📜 Tangling scripts..."
	$(EMACS) --batch -l org --eval "(progn (find-file \"scripts.org\") (org-babel-tangle))"
	@chmod +x ~/.local/bin/*.sh
	@echo "✅ Scripts tangled and made executable."

clean:
	@echo "🧹 Cleaning generated files..."
	@rm -f ~/.config/doom/init.el ~/.config/doom/config.el ~/.config/doom/packages.el ~/.config/doom/custom.el ~/.zshrc ~/.local/bin/*.sh || true
	@echo "✅ Clean completed."
