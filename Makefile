static_build:
	rm -rf /tmp/burrow-src
	cp -r . /tmp/burrow-src
	rm -rf /tmp/burrow-src/dist-newstyle
	docker run --rm -v /tmp/burrow-bin-result:/host-bin -v /tmp/burrow-src:/home/build -it fossa/haskell-static-alpine:ghc-8.10.4 /bin/sh -c "cd /home/build && cabal update && cabal build --enable-executable-static"
	echo "Built to /tmp/burrow-src/dist-newstyle/build/x86_64-linux/ghc-8.10.4/burrow-0.1.0.0/x/burrow/build/burrow/burrow"
