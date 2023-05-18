GUILE=guix shell guile guile-fibers-next -- guile

repl:
	${GUILE} -L ./src -L ./tests --listen

server:
	${GUILE} -L ./src -c "((@ (nrepl-server) run-nrepl-server))"
