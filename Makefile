build:
	export NIXPKGS_ALLOW_BROKEN=1
	export LC_ALL=C.UTF-8
	nix-build release.nix

run: build
	result/bin/burrow --source example-gopherhole --destination built --spacecookie

repl:
	nix-shell --pure shell.nix --run \
		"cabal repl lib:burrow"

shell:
	nix-shell shell.nix

shell-pure:
	nix-shell --pure shell.nix

external-shell:
	nix-shell external.nix

.PHONY: build run repl shell shell-pure external-shell
