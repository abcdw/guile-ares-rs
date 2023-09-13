GUILE=guix shell guile guile-fibers -- guile
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

check: check-evaluation check-integration

check-module:
	${GUILE} -L ./src -L ./tests -L ${GIDER} \
	-c "((@ (gider test-runners) run-module-tests) \
	(resolve-module '${TEST_MODULE}))"

check-evaluation:
	make check-module TEST_MODULE="(nrepl server evaluation-test)"

check-integration:
	make check-module TEST_MODULE="(integration-test)"

dumb-client:
	./tests/dumb-client.sh ${NREPL_PORT}
