# burrow Docker server

Docker server setup for automatically building a
[gopherhole](https://en.wikipedia.org/wiki/Gopher_(protocol)) with
[Burrow](https://github.com/hyperrealgopher/burrow) every time a commit is
pushed and serving it with
[Spacecookie](https://github.com/sternenseemann/spacecookie).

A `Makefile` is included for your convenience:

  * `make create_network`: You should run this first. Creates the Docker network the container will use.
  * `make build`: Build the Docker container.
  * `make run`: Run the Docker container as a daemon.
  * `make shell`: Open the shell of the currently running Docker container.

You can use it from project root like this:

```
make -f ./docker/Makefile run
```

## Configure `spacecookie.json`

...

## In case of a problem when building

If `make build` fails you can try this command instead:

```
docker build --no-cache -t spacecookie .
```

## Setup client

Configure the machine you'll be editing the gopherhole on and pushing commits from.

Put this in your `~/.ssh/config` (set `HostName` to the host/domain/IP address
where the Docker daemon is hosted and set `IdentityFile` to your private key
you want to use for pushing commits):

```
Host gopherhole
	HostName 6pb7ikzn72tzuhg6pdlkogwgo4yslf5r6mvktdjzyssfry2uvzxthpyd.onion
	User git
	IdentityFile ~/.ssh/id_rsa_hgopher
```

Be sure to copy the public key (`*.pub`) for the private key you specified into
the directory with the `Dockerfile` and `Makefile` (the root of the repo)
*before* running `make build` (building the container).

### Setup the gopherhole/repo

You'll need to learn about making gopherholes with
[Burrow](https://github.com/hyperrealgopher/burrow).

```
$ cd myproject
$ git init
$ git add .
$ git commit -m 'Initial commit'
$ git remote add origin git@gopherhole:/srv/git/gopherhole.git
$ git push origin master
```

If you're using Tor you'll want to do `torsocks push origin master`.

You can pull from the repo.  Others can clone it down and push changes back up
just as easily (if the have the same private key [I'll fix this soon by adding
all the `*.pub` keys in repo root).

```
$ git clone git@gopherhole:/srv/git/project.git
$ cd project
$ vim README
$ git commit -am 'Fix for README file'
$ git push origin master
```

## Tor

In case you want to serve via Tor edit your `/etc/tor/torrc/`:

```
HiddenServicePort 70 172.18.0.68:70
HiddenServicePort 22 172.18.0.68:22
```

## Software used

Some info about the software used.

### Spacecookie version

This repo is setup so the latest GitHub main branch from Spacecookie is always
used. If you want to upgrade to the latest version simply rebuild the image
like this:

### Burrow version

...
