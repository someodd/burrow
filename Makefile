# Example: make static_build GHC_VERSION=8.10.4 BURROW_VERSION=0.1.0.0
# We use alpine edge because it has musl by default for libc which a static
# libc is needed for static compilation to happen.
static_build:
	rm -rf /tmp/burrow-src
	cp -r . /tmp/burrow-src
	rm -rf /tmp/burrow-src/dist-newstyle
	docker run --rm -v /tmp/burrow-bin-result:/host-bin -v /tmp/burrow-src:/home/build "fossa/haskell-static-alpine:ghc-${GHC_VERSION}" /bin/sh -c "cd /home/build && cabal update && cabal build --enable-executable-static"
	echo "Built to /tmp/burrow-src/dist-newstyle/build/x86_64-linux/ghc-${GHC_VERSION}/burrow-${BURROW_VERSION}/x/burrow/build/burrow/burrow"

release:
	sed -i "s/^version:[[:space:]]\+[0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+/version: ${BURROW_VERSION}/g" burrow.cabal
	make static_build
