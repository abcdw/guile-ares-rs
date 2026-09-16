GUIXTM=guix time-machine --unsafe-channel-evaluation -C ./env/guix/ares/env/guix/channels.scm
GUILE=$(GUIXTM) -- shell guile guile-fibers \
--pure --preserve=.*LANG.* -- guile
EMACS=$(GUIXTM) -- shell emacs emacs-ox-html-stable-ids -- emacs
HUT=$(GUIXTM) -- shell hut -- hut
GUIX=$(GUIXTM) --
LOAD_PATHS=-L src/guile -L tests/guile -L dev/guile
SRFI_269_R7RS_LOAD_PATHS=-L src/srfi-269/r7rs \
-L tests/srfi-269/r7rs -L src/guile
SRFI_269_GUILE_LOAD_PATHS=-L src/srfi-269/guile \
-L tests/srfi-269/guile -L src/guile
GUILE_DEV=${GUILE} $(LOAD_PATHS)
GUILE_SRFI_269_R7RS=${GUILE} $(SRFI_269_R7RS_LOAD_PATHS)
GUILE_SRFI_269_GUILE=${GUILE} $(SRFI_269_GUILE_LOAD_PATHS)
REPORTER?=compact
SCHEDULER?=non-dev

repl: server

server:
	${GUILE_DEV} -c \
	"((@ (ares server) run-nrepl-server) #:dev? #t)"

ares: server

check: check-suitbl check-srfi-269 suitbl
	${GUILE_DEV} \
	-c "((@ (ares srfi-64 test-runners) run-project-tests-cli))"

check-project-junit-output:
	${GUILE_DEV} \
	-c "((@ (suitbl-test-runner) run-project-tests-junit-output))"

check-suitbl:
	${GUILE_DEV} \
	-c "((@ (suitbl-test-runner) run-suitbl-tests))"

check-srfi-269:
	${GUILE_SRFI_269_R7RS} \
	-c "((@ (ares srfi-64 test-runners) run-module-tests) \
(resolve-module '(srfi srfi-269-test)))"

check-srfi-269-guile:
	${GUILE_SRFI_269_GUILE} \
	-c "((@ (srfi-269-test-runner) run-tests))"

suitbl:
	${GUILE_DEV} \
	-e '(ares scripts ares-suitbl)' \
	-s ./src/guile/ares/scripts/ares-suitbl.scm \
	-r '$(REPORTER)' \
	$(if $(SCHEDULER),-s '$(SCHEDULER)') \
	-- $(LOAD_PATHS)

suitbl-specimens:
	${MAKE} suitbl \
		REPORTER='$(REPORTER)' \
		SCHEDULER='(make-module "specimens-test")'

suitbl-specimens-minimal:
	${MAKE} suitbl-specimens REPORTER=minimal

suitbl-specimens-compact:
	${MAKE} suitbl-specimens REPORTER=compact

suitbl-specimens-base:
	${MAKE} suitbl-specimens REPORTER=base

suitbl-specimens-junit:
	${MAKE} suitbl-specimens REPORTER=junit

README.html: README
	${EMACS} -Q --batch -l docs/html-export-config.el README \
	--funcall org-html-export-to-html

deploy-README.html: README.html
	${HUT} git update --readme README.html \
	--repo https://git.sr.ht/~abcdw/guile-ares-rs

clean:
	rm README.html

clean-cache:
	find $${XDG_CACHE_HOME:-$$HOME/.cache}/guile/ccache \
	  -mindepth 1 -maxdepth 1 -type d \
	  -exec rm -rf {}$(CURDIR) \;
