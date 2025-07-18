GUIXTM=guix time-machine -C ./env/guix/ares/env/guix/channels.scm
GUILE=$(GUIXTM) -- shell guile guile-fibers guile-custom-ports -- guile
EMACS=$(GUIXTM) -- shell emacs emacs-ox-html-stable-ids -- emacs
HUT=$(GUIXTM) -- shell hut -- hut
GUIX=$(GUIXTM) --
GUILE_DEV=${GUILE} -L ./src/guile -L ./test/guile -L ./dev/guile

repl: server

server:
	${GUILE_DEV} -c \
	"((@ (ares server) run-nrepl-server))"

ares: server

check: check-suitbl
	${GUILE_DEV} \
	-c "((@ (ares srfi-64 test-runners) run-project-tests-cli))"

check-suitbl:
	${GUILE_DEV} \
	-c "((@ (ares suitbl-test) run-tests))"

check-test:
	${GUILE_DEV} \
	-c "((@ (ares srfi-64 test-runners) run-test) \
	(@@ (ares evaluation-test) test-evaluation-thread-manager))"

check-module:
	${GUILE_DEV} \
	-c "((@ (ares srfi-64 test-runners) run-module-tests) \
	(resolve-module '${TEST_MODULE}))"

check-evaluation:
	make check-module TEST_MODULE="(ares evaluation-test)"

check-bootstrap:
	make check-module TEST_MODULE="(ares nrepl bootstrap-test)"

check-integration:
	make check-module TEST_MODULE="(integration-test)"

check-topological-sort:
	make check-module TEST_MODULE="(ares topological-sort-test)"

README.html: README
	${EMACS} -Q --batch -l docs/html-export-config.el README \
	--funcall org-html-export-to-html

deploy-README.html: README.html
	${HUT} git update --readme README.html \
	--repo https://git.sr.ht/~abcdw/guile-ares-rs

clean:
	rm README.html
