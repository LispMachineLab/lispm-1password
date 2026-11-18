EMACS ?= emacs
# LispM UI (including Kit), with which 1Password declares its commands:
# a checkout beside this one by default.
LISPM_UI ?= ../lispm-ui
LOAD_PATH = -L . -L $(LISPM_UI)

FILES = lispm-1password.el lispm-1password-ui.el

.PHONY: test compile

test:
	$(EMACS) -Q --batch $(LOAD_PATH) --eval '(setq native-comp-enable-subr-trampolines nil)' \
	  -l test/lispm-1password-test.el -f ert-run-tests-batch-and-exit

# A check: the .elc files are removed again, as they would shadow the
# sources of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch -L . -L $(LISPM_UI) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(FILES); \
	status=$$?; rm -f *.elc; exit $$status
