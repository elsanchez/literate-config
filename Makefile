# Makefile to tangle literate config files for Doom Emacs and Zsh
# Enhanced with backup system for safe configuration management

EMACS=emacs
ORG_FILES=$(wildcard *.org)
BACKUP_DIR=~/.config-backups
TIMESTAMP=$(shell date +"%Y%m%d_%H%M%S")

# Main targets
all: tangle
all-safe: backup tangle

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

# Backup system targets
backup: backup-init
	@echo "💾 Creating configuration backup..."
	@mkdir -p $(BACKUP_DIR)
	@if [ -f ~/.config/doom/init.el ] || [ -f ~/.config/doom/config.el ] || [ -f ~/.zshrc ] || [ -f ~/.config/tmux/tmux.conf.local ]; then \
		cp -f ~/.config/doom/init.el $(BACKUP_DIR)/ 2>/dev/null || true; \
		cp -f ~/.config/doom/config.el $(BACKUP_DIR)/ 2>/dev/null || true; \
		cp -f ~/.config/doom/packages.el $(BACKUP_DIR)/ 2>/dev/null || true; \
		cp -f ~/.config/doom/custom.el $(BACKUP_DIR)/ 2>/dev/null || true; \
		cp -f ~/.zshrc $(BACKUP_DIR)/ 2>/dev/null || true; \
		cp -f ~/.config/tmux/tmux.conf.local $(BACKUP_DIR)/ 2>/dev/null || true; \
		mkdir -p $(BACKUP_DIR)/scripts 2>/dev/null || true; \
		cp -f ~/.local/bin/*.sh $(BACKUP_DIR)/scripts/ 2>/dev/null || true; \
		cd $(BACKUP_DIR) && git add . && git commit -m "Backup $(TIMESTAMP)" >/dev/null 2>&1 || true; \
		echo "✅ Backup created: $(TIMESTAMP)"; \
	else \
		echo "⚠️  No existing configurations found to backup"; \
	fi

backup-init:
	@if [ ! -d $(BACKUP_DIR)/.git ]; then \
		echo "🔧 Initializing backup repository..."; \
		mkdir -p $(BACKUP_DIR); \
		cd $(BACKUP_DIR) && git init >/dev/null 2>&1; \
		cd $(BACKUP_DIR) && git config user.name "Literate Config Backup" >/dev/null 2>&1; \
		cd $(BACKUP_DIR) && git config user.email "backup@literate-config.local" >/dev/null 2>&1; \
	fi

tangle-safe: backup tangle

list-backups:
	@if [ -d $(BACKUP_DIR)/.git ]; then \
		echo "📋 Available configuration backups:"; \
		cd $(BACKUP_DIR) && git log --oneline --format='%C(yellow)%h%C(reset) %C(green)%ad%C(reset) %s' --date=short | head -10; \
	else \
		echo "❌ No backup repository found. Run 'make backup' first."; \
	fi

restore-backup:
	@if [ -d $(BACKUP_DIR)/.git ]; then \
		echo "⚠️  This will overwrite current configurations!"; \
		echo "📋 Available backups:"; \
		cd $(BACKUP_DIR) && git log --oneline --format='%h %ad %s' --date=short | head -5; \
		echo "\n💡 To restore a specific backup, use: cd $(BACKUP_DIR) && git checkout <commit-hash> -- ."; \
		echo "💡 Then manually copy files to their destinations"; \
	else \
		echo "❌ No backup repository found."; \
	fi

clean-backups:
	@if [ -d $(BACKUP_DIR)/.git ]; then \
		echo "🧹 Cleaning old backups (keeping last 10)..."; \
		cd $(BACKUP_DIR) && git log --format='%H' | tail -n +11 | xargs -r git branch -D 2>/dev/null || true; \
		echo "✅ Old backups cleaned"; \
	else \
		echo "❌ No backup repository found."; \
	fi

clean:
	@echo "🧹 Cleaning generated files..."
	@rm -f ~/.config/doom/init.el ~/.config/doom/config.el ~/.config/doom/packages.el ~/.config/doom/custom.el ~/.zshrc ~/.config/tmux/tmux.conf.local ~/.local/bin/*.sh || true
	@echo "✅ Clean completed."

# Help and information
help:
	@echo "📚 Literate Configuration Makefile Help"
	@echo ""
	@echo "Main targets:"
	@echo "  all          - Tangle all .org files (default)"
	@echo "  all-safe     - Create backup then tangle (recommended)"
	@echo "  tangle       - Tangle .org files to config files"
	@echo "  tangle-safe  - Backup then tangle (alias for all-safe)"
	@echo "  scripts      - Tangle only scripts"
	@echo ""
	@echo "Backup management:"
	@echo "  backup       - Create backup of current configurations"
	@echo "  list-backups - List available backups"
	@echo "  restore-backup - Show restore instructions"
	@echo "  clean-backups - Remove old backups (keep last 10)"
	@echo ""
	@echo "Maintenance:"
	@echo "  clean        - Remove generated config files"
	@echo "  help         - Show this help message"
	@echo ""
	@echo "💡 Use 'make all-safe' for safe configuration updates"

.PHONY: all all-safe tangle tangle-safe scripts backup backup-init list-backups restore-backup clean-backups clean help