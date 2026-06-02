EMACS ?= emacs

LOAD_PATH = -L . -L ../avy -L ../evil -L ../../build/avy -L ../../build/evil

.PHONY: test
test:
	$(EMACS) -Q --batch $(LOAD_PATH) -l test/evil-easymotion-test.el -f ert-run-tests-batch-and-exit
