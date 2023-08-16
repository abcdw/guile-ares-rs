GUILE=guix shell guile guile-fibers-next -- guile

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

check:
	${GUILE} -L ./src -L ./tests -c "((@ (integration-test) tmp-fn))"

dumb-client:
	./tests/dumb-client.sh ${NREPL_PORT}
