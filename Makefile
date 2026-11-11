EMACS ?= emacs

.PHONY: lint test check clean helper

helper:
	cargo build --release --locked --manifest-path duckling-cli/Cargo.toml

## Run all checks (lint + test) — use before pushing
check: lint test

## Byte-compile and checkdoc
lint:
	$(EMACS) --batch -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile org-natural-dates.el org-natural-dates-duckling.el
	$(EMACS) --batch -L . \
	  --eval '(setq checkdoc-spellcheck-documentation-flag nil)' \
	  --eval '(checkdoc-file "org-natural-dates.el")' \
	  --eval '(when (get-buffer "*Checkdoc Status*") (with-current-buffer "*Checkdoc Status*" (unless (string= (buffer-string) "") (message "%s" (buffer-string)) (kill-emacs 1))))'

## Run ERT tests
test:
	$(EMACS) --batch -L . \
	  -l org-natural-dates-test.el -l org-natural-dates-duckling-test.el \
	  -f ert-run-tests-batch-and-exit

## Remove byte-compiled files
clean:
	rm -f *.elc
