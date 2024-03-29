GUIXTM=guix time-machine --commit=8b249a1be69874d029cd8a2d2ff170f9007c5fc4
GUILE=$(GUIXTM) -- shell guile-next guile-fibers -- guile
EMACS=$(GUIXTM) -- shell emacs emacs-ox-html-stable-ids -- emacs
HUT=$(GUIXTM) -- shell hut -- hut
GIDER=`guix build -e '(@ (rde packages emacs-xyz) emacs-gider)'`/share/emacs/site-lisp/gider-0.1.0/src
NREPL_PORT=7888

repl:
	${GUILE} -L ./src -L ./tests --listen

nrepl-proxy:
	guix shell openjdk clojure-tools -- \
	clj -Sdeps \
	'{:deps {com.lambdaisland/nrepl-proxy {:mvn/version "0.2.8-alpha"}}}' \
	-X lambdaisland.nrepl-proxy/start :port 1234 :attach ${NREPL_PORT}

server:
	${GUILE} -L ./src -c \
	"((@ (nrepl server) run-nrepl-server) #:port ${NREPL_PORT})"

check: check-evaluation check-bootstrap check-integration

check-module:
	${GUILE} -L ./src -L ./tests -L ${GIDER} \
	-c "((@ (gider test-runners) run-module-tests) \
	(resolve-module '${TEST_MODULE}))"

check-evaluation:
	make check-module TEST_MODULE="(nrepl server evaluation-test)"

check-bootstrap:
	make check-module TEST_MODULE="(nrepl bootstrap-test)"

check-integration:
	make check-module TEST_MODULE="(integration-test)"

dumb-client:
	./tests/dumb-client.sh ${NREPL_PORT}

README.html: README
	${EMACS} -Q --batch -l docs/html-export-config.el README \
	--funcall org-html-export-to-html

deploy-README.html: README.html
	${HUT} git update --readme README.html \
	--repo https://git.sr.ht/~abcdw/guile-ares-rs
