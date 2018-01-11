EMACS ?= emacs
CASK ?= cask
PKG_DIR := $(shell cd features/project ; ${CASK} package-directory)

all: test

test: clean-elc
	${MAKE} ecukes
	${MAKE} compile
	${MAKE} ecukes
	${MAKE} clean-elc

ecukes: elpa
	${CASK} exec ecukes --script --reporter spec features

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile ffe-config.el

clean-elc:
	rm -f ffe-config.elc

elpa: ${PKG_DIR}
${PKG_DIR}:
	$(shell cd features/project ; ${CASK} install)
	touch $@

.PHONY:	all test compile clean-elc ecukes
