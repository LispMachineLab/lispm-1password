EMACS ?= emacs
# lisp/ of LispM Desktop (which needs LispM Browser's), checkouts beside
# this one by default.  1Password needs neither; with them on the load
# path, the test of how it works with the desktop runs too.
LISPM_DESKTOP ?= ../lispm-desktop/lisp
LISPM_BROWSER ?= ../lispm-browser/lisp
LOAD_PATH = -L . $(foreach dir,$(wildcard $(LISPM_DESKTOP) $(LISPM_BROWSER)),-L $(dir))

FILES = lispm-1password.el lispm-1password-ui.el

.PHONY: test compile

test:
	$(EMACS) -Q --batch $(LOAD_PATH) --eval '(setq native-comp-enable-subr-trampolines nil)' \
	  -l test/lispm-1password-test.el -f ert-run-tests-batch-and-exit

# A check: the .elc files are removed again, as they would shadow the
# sources of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(FILES); \
	status=$$?; rm -f *.elc; exit $$status
