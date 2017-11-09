all:
	make -C examples/local
	make -C examples/remote
	make -C examples/pulling-worker/master
	make -C examples/pulling-worker/worker
