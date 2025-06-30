# ✓ Validation Checklist - Doom Configuration

**CRITICAL**: This checklist MUST be followed for ANY changes to configuration files.

---

## 📋 Pre-Change Checklist

- [ ] **Run isolated test**: `doom-test-config`
- [ ] **Check syntax**: `emacs --batch --eval "(progn (find-file \"doom-config.org\") (check-parens))"`
- [ ] **Verify doom sync**: `doom sync`
- [ ] **All tests pass?** If NO → STOP. Do not proceed.

---

## 🔧 During Changes

- [ ] **Edit ONLY .org files** (never edit generated files)
- [ ] **Keep functions at top level** (no nested defuns)
- [ ] **Check parentheses balance** after each function
- [ ] **Save frequently** but don't tangle yet

---

## 📋 Post-Change Checklist

- [ ] **Tangle configuration**: `make` or `org-babel-tangle`
- [ ] **Re-run isolated test**: `doom-test-config`
- [ ] **Validate generated file**: `emacs --batch --eval "(progn (find-file \"~/.config/doom/config.el\") (check-parens))"`
- [ ] **Stage configuration**: `doom-stage-config`
- [ ] **Test key functions**: 
  - [ ] `SPC r d` - Reload works?
  - [ ] `SPC r t t` - Test works?
  - [ ] `doom sync` - Completes without errors?

---

## 🚀 Deployment Checklist

- [ ] **All tests pass?**
- [ ] **Backup created?**: `make backup`
- [ ] **Ready to apply?**: `SPC r d` → `[d]irect`
- [ ] **Verify functions work** in live environment
- [ ] **Commit changes**: Only .org files, not generated

---

## 🚨 If Something Breaks

1. **DON'T PANIC**
2. **Check backups**: `config-list-backups`
3. **Rollback**: `doom-rollback` or `config-restore`
4. **Review error**: What exactly failed?
5. **Document issue** in LESSONS_LEARNED.md

---

## 🔴 Red Flags - STOP if you see:

- "Unmatched bracket or quote"
- "End-of-file" errors
- Functions defined inside other functions
- Doom sync hanging or failing
- Any syntax validation errors

---

## ✅ Quick Commands Reference

```bash
# Test before changes
doom-test-config

# Quick syntax check
emacs --batch --eval "(progn (find-file \"file.el\") (check-parens))"

# Full validation
doom-test-config && doom sync && echo "✅ All good!"

# Emergency rollback
doom-rollback
```

---

**Remember**: It's better to test 10 times than to break once. The testing tools exist for a reason - USE THEM!