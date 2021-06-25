# Example: make static_build GHC_VERSION=8.10.4
# We use alpine edge because it has musl by default for libc which a static
# libc is needed for static compilation to happen.
static_build:
	export BURROW_VERSION_CABAL='$(sed -n "s/^version:[[:space:]]\+\([0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\)\+/\1/p" burrow.cabal)'
	rm -rf /tmp/burrow-src
	cp -r . /tmp/burrow-src
	rm -rf /tmp/burrow-src/dist-newstyle
	docker run --rm -v /tmp/burrow-bin-result:/host-bin -v /tmp/burrow-src:/home/build "fossa/haskell-static-alpine:ghc-${GHC_VERSION}" /bin/sh -c "cd /home/build && cabal update && cabal build --enable-executable-static"
	tar -czf "burrow-${BURROW_VERSION_CABAL}-x86_64-linux.tar.gz" -C "/tmp/burrow-src/dist-newstyle/build/x86_64-linux/ghc-${GHC_VERSION}/burrow-${BURROW_VERSION_CABAL}/x/burrow/build/burrow/" burrow

release:
	export BURROW_VERSION_CABAL='$(sed -n "s/^version:[[:space:]]\+\([0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\)\+/\1/p" burrow.cabal)'
	make static_build
	# make debian package
	mkdir -p "/tmp/burrow_${BURROW_VERSION_CABAL}_amd64/usr/local/bin/"
	mkdir -p "/tmp/burrow_${BURROW_VERSION_CABAL}_amd64/DEBIAN"
	echo "Package: burrow\nVersion: ${BURROW_VERSION_CABAL}\nArchitecture: amd64\nMaintainer: hyperrealgopher <hyperrealgopher@protonmail.ch>\nDescription: Static site builder, but for gopherholes.\n Manage phlogs with tags, use the Markdown renderer and Mustache templating system." > "/tmp/burrow_${BURROW_VERSION_CABAL}_amd64/DEBIAN/control"
	dpkg-deb --build --root-owner-group "/tmp/burrow_${BURROW_VERSION_CABAL}_amd64"
