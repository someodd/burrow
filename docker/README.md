# Burrow Docker setup

Docker config to serve a gopherhole built with Burrow. Features:

* Special modes (see section below)!

  * Automatically upgrade Burrow (this is an unstable option for those living on the edge)
  * Periodically check a remote repo and build from that (ex: push to GitHub, wait, see gopherhole update)

* git server you can push to, which will trigger a rebuild+restart of your gopherhole

This Docker config is set up so you run commands from the project root.

## Special modes

Use the optional `--CRON_TIME` Docker build arg if you want to override the cron time.

Some variables to set in an `.env` (please see the example `.env` file included):

* `AUTO_BURROW_UPGRADE`: If set to `true` try to (automatically) upgrade Burrow on the `CRON_TIME` schedule
* `GOPHERHOLE_REMOTE_URL`: If set to a remote git repository HTTP(s) URI, ensure "everything" is up-to-date with it.

  * An optional `GOPHERHOLE_REMOTE_BRANCH` (defaulting to `main`) defines the remote branch to check against.

## Build and run

Make sure you've created a Docker network (example used below is `--net docker_default`).

```
docker build --build-arg CRON_TIME="0 * * * *" -t spacecookie -f docker/Dockerfile docker/ --no-cache
docker run --env-file docker/.env -d --restart=always --net docker_default --hostname=spacecookie --ip=172.18.0.68 -p 7071:7071 -p 2222:22 spacecookie
```
## Makefile

A `Makefile` is included for your convenience:

  * `make create_network`: You should run this first. Creates the Docker network the container will use.
  * `make build`: Build the Docker container.
  * `make run`: Run the Docker container as a daemon.
  * `make run_bigport`: Run with bigger ports than the defaults for SSH and Gopher.
  * `make shell`: Open the shell of the currently running Docker container.

You must use it from project root like this:

```
make -f ./docker/Makefile build
```

The `Makefile` may get phased out.

## Client-side configuration

The Docker container works by managing the gopherhole as a git repository, in a sense.

While you can simply push to GitHub (if using `SYNC_MODE), you can also push directly to
the gopherhole repo in the container.

This section is devoted to configuring the machine where you edit the gopherhole on and
push commits from.

### Client-side SSH configuration

Configure the machine you'll be editing the gopherhole on and pushing commits from.

Add an entry to `~/.ssh/config`, but set `Hostname` to the address/domain the Docker
container is accessible from. Also associate a private key (`IdentityFile`):

```
Host gopherhole
	HostName 192.168.1.25
	User git
	IdentityFile ~/.ssh/id_rsa_someodd
```

Copy the public key (`.pub`, associated with the private key you specified above) to the
directory containing the `Dockerfile`.

### Client-side gopherhole repo setup

You'll need to learn about making gopherholes with
[Burrow](https://github.com/someodd/burrow). Please look at [my personal
gopherhole](https://github.com/someodd/personal-gopherhole) for an example of what
`myproject` below should look like.

```
$ cd myproject
$ git init
$ git add .
$ git commit -m 'Initial commit'
$ git remote add gopherhole git@gopherhole:/srv/git/gopherhole.git
$ git remote add origin git@github.com:someodd/personal-gopherhole.git # optional! set to GH repo you made for your gopherhole
$ git push # optional! pushes to github
$ git push gopherhole
```