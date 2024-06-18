GUIXTM=guix time-machine -C ./rde/channels-lock.scm
GUILE=$(GUIXTM) -- shell guile-next guile-fibers -- guile
EMACS=$(GUIXTM) -- shell emacs emacs-ox-html-stable-ids -- emacs
HUT=$(GUIXTM) -- shell hut -- hut
GUIX=$(GUIXTM) --
GUILE_DEV=${GUILE} -L ./src/guile -L ./test/guile -L ./dev/guile

repl: server

server:
	${GUILE_DEV} -c \
	"((@ (nrepl server) run-nrepl-server))"

ares-rs: server

check:
	${GUILE_DEV} \
	-c "((@ (ares srfi-64 test-runners) run-project-tests))"

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
	make check-module TEST_MODULE="(nrepl bootstrap-test)"

check-integration:
	make check-module TEST_MODULE="(integration-test)"

README.html: README
	${EMACS} -Q --batch -l docs/html-export-config.el README \
	--funcall org-html-export-to-html

deploy-README.html: README.html
	${HUT} git update --readme README.html \
	--repo https://git.sr.ht/~abcdw/guile-ares-rs
