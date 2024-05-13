EMACS ?= emacs

TEST_DIR=$(shell pwd)/test

# Run all tests by default.
MATCH ?=

.PHONY: test

test:
	$(EMACS) --batch -L lisp/utils/ -L lisp/ -L test/ -l all-tests.el -eval '(ert-run-tests-batch-and-exit "$(MATCH)")'
