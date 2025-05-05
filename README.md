# Burrow: Make Gopherholes

Archived: I am no longer maintaining this project because I created [Bore](https://github.com/someodd/bore)! Please check it out!

Burrow is to gopherholes as static site builders are to websites.

Burrow is a *static site generator,* but for gopherholes. A *gopherhole* is a place in *gopherspace* accessed through the [Gopher protocol](https://en.wikipedia.org/wiki/Gopher_%28protocol%29)).

I made my gopherhole with burrow, so it's an example:

  * gopher://gopher.someodd.zip:7071/
  * https://github.com/someodd/personal-gopherhole
  * This functional example uses this repo's Docker config, allow me to push to the above
    repo, and the repo that is used to build my gopherhole (Docker setup is a git + gopher
    server)

There's a `.deb` (Ubuntu, Debian) available in the releases.

Written in Haskell. If you're looking for very similar software, with coincidentally the exact same name, please check out [James Tomasino's Burrow](https://github.com/jamestomasino/burrow).

## Conceptual

You should know these terms:

* gopherhole
* gopherhole project
* gophermap/menu
* gopherspace

### gopherhole project overview

A "gopherhole project" is simply a directory that looks like this:

```
╭━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╮
┃ ⚘ PROJECT ROOT ⚘                              ┃
┃ ┣━━ 📂 data/                                  ┃
┃ ┃   ┣━━ 📂 fonts/  [BMF fonts]                ┃
┃ ┃   ┗━━ 📜 burrow.toml  [Main config]         ┃
┃ ┣━━ 📂 burrowsrc/  [Gopherhole content]       ┃
┃ ┗━━ 📂 templates/  [Reusable document bits]   ┃
╰━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╯
```

Note that you can turn this gopherhole project into a git repo... work with Docker...

The software *does* change directory to the project root and assume project root as the
path that some things are relative to. By default the project root is assumed to be the
directory containing `data/burrow.toml` (like when you point to it with `--config`).

All of this "compiles" to a specified output directory, to be served via the Gopher
Protocol.

### `data/burrow.toml`

Perhaps the most important file to look over and change yourself. Rules for building the
gopherhole project, but also for serving a gopherhole.

Any relative paths are assumed to be relative to the project root.

### `data/fonts/`

ASCII art fonts in `.bmf` format (Burrow M [I forgot] Font). These are currently used when
rendering headers from Markdown files, but I imagine soon I'll make a function so you can
arbitrarily make ASCII art text.

`.bmf` font spec is simple: on one line is a character you wish to represent, followed by the
ASCII art for the character. Each line for the character must be of the same length. Each
character in the font must be of the same number of lines. Separate character definitions
with blank lines (`\n\n`).

### `templates/`

Re-usable document bits. You can use these bits like macros, to:

  * jam some file you wrote *inside* of a template
  * include some template into a file you wrote

You can call upon templates using Frontmatter, but also Moustache.

### `burrowsrc/`

All the files to be served from your gopherhole, which burrow will either parse or copy.
Use whatever directory structure in there.

The rules for parsing a regular text or Markdown file in a gopherholeproject, into either (for gopherspace) a regular text file or a gophermap/menu, are configured like so:

* In `burrow.toml`
* In the file's Frontmatter: this *should* override everythinge else
* The file's extension

You may want to read the section(s) on Frontmatter and templating for these files as well
and reading `burrow.toml`.

## Quick Start

This repo contains an example gopherhole project, run this command and then visit
`localhost:7070` in `gopher` (make sure `gopher` is installed):

```
burrow serve --config data/burrow.toml --watch
gopher -p "/" localhost 7070
```

Try editing the files in `burrowsrc/` and revisiting the gopherhoole.

There are many different Gopher Protocol clients, my favorite GUI client is
[Lagrange](https://gmi.skyjake.fi/lagrange/) (Linux, Mac, iOS, Android, more?)--try using
it to visit [gopher://localhost:7070/](gopher://localhost:7070/).

For an actual production server and even for local testing you may want to check this
project's Docker configuration in `docker/`, there's a `README.md` to help you get
started. It has some neat devops-type features.

You can download a binary from the releases page.

## Some features

* Parse markdown, using commonmark
* Config-based parser, server, and other behavior: `burrow.toml`
* Front matter
* Mustache templating with extended features
* Blogging/phlogging
* ASCII-art font system
* `.gophermap` support, also this is a `--spacecookie` and burrow setting (which files to
  make into the main menu for the directory, similar to `index.html` for web)

### Front matter

[Jekyll-like front matter support](https://jekyllrb.com/docs/front-matter/). You can tag posts for a phlog, control the rendering process for the file, and more, all through the front matter of the file.

Here's an example of various front matter entries specifically supported by Burrow:

```markdown
---
published: 2021-06-01T05:44
updated: June 13th at 10pm
author: Some Author's Name
type: post
variables: {"someVariable": "Some text to replace someVariable.", "foo": "bar"}
skipMustache: false
skipMarkdown: false
parentTemplate: post
renderAs: menu
---

Here's the content of my post that supports Markdown and Mustache.
```

### Mustache

Use [Mustache](https://mustache.github.io/) for templating. In addition to simple
templating, it also comes pre-loaded with some functions for ASCII-art-ifying files.

There are built in lambdas for your convenience:

  * `{{#columnate2}}this text will be justified and broken up like a newspaper{{/columnate2}}`
  * `{{#justify}}this text will be justified{{/justify}}`
  * `{{#justify2}}this text will be justified using a different algorith, I think{{/justify2}}`

I imagine more is to come and hopefully even more beautiful ASCII art can be had. I
encourage people, here most of all, to make open source contributions.

### Blogging (phlogging) features

Indexes will be generated as gophermaps for posts which are tagged and have all
the required front matter (like `published`, `title`, and `type: post`).

Atom feeds will be generated for each index (tag indexes, the main feed, and
tag summaries).

## Other notes

This software has primarily been tested with
[spacecookie](https://github.com/sternenseemann/spacecookie) (Gopher server). Although now
techically using an integrated fork of that software in Burrow.  You may want to look at
[my spacecookie Docker repo](https://github.com/someodd/docker-spacecookie). Currently,
Burrow relies heavily on the `.gophermap` behavior outlined in these documents:

* [spacecookie.gophermap(5)](https://sternenseemann.github.io/spacecookie/spacecookie.json.5.html)
* [Bucktooth's gophermap tutorial](http://gopher.floodgap.com/gopher/gw.lite?=gopher.floodgap.com+70+302f6275636b2f6462726f7773653f666171757365203161)
* [Bucktooth](http://gopher.floodgap.com/gopher/gw.lite?=gopher.floodgap.com+70+312f6275636b).
* [pygopherd(8)](https://manpages.debian.org/unstable/pygopherd/pygopherd.8.en.html)
* [pygopherd](https://github.com/jgoerzen/pygopherd)

Other good things to read about and know:

* [mustache](https://mustache.github.io/mustache.5.html)
* [CommonMark](https://commonmark.org/): a great Markdown standard (used by GitHub, Reddit, Stack Overflow)
* You should know about [the Gopher protocol](https://en.wikipedia.org/wiki/Gopher_%28protocol%29)

## Big parsing example

Turn this:

```markdown
---
title: Example phlog post
published: 2021-06-12
updated: June 13th, 2021 at 10pm
author: someodd
type: post
tags: foo bar
variables: {"someVariable": "Some text to replace someVariable.", "foo": "bar"}
parentTemplate: post.txt
renderAs: menu
---

This is an example phlog post.

{{ someVariable }}

## Columnate Example

{{#columnate2}}
The Gopher protocol /ˈɡoʊfər/ is a communications protocol designed for distributing, searching, and retrieving documents in Internet Protocol networks. The design of the Gopher protocol and user interface is menu-driven, and presented an alternative to the World Wide Web in its early stages, but ultimately fell into disfavor, yielding to the Hypertext Transfer Protocol (HTTP) The Gopher ecosystem is often regarded as the effective predecessor of the World Wide Web.[1]
The protocol was invented by a team led by Mark P. McCahill[2] at the University of Minnesota. It offers some features not natively supported by the Web and imposes a much stronger hierarchy on the documents it stores. Its text menu interface is well-suited to computing environments that rely heavily on remote text-oriented computer terminals, which were still common at the time of its creation in 1991, and the simplicity of its protocol facilitated a wide variety of client implementations. More recent Gopher revisions and graphical clients added support for multimedia.[citation needed] Gopher was preferred by many network administrators for using fewer network resources than Web services.[3]
Gopher's hierarchical structure provided a platform for the first large-scale electronic library connections.[4] The Gopher protocol is still in use by enthusiasts, and although it has been almost entirely supplanted by the Web, a small population of actively-maintained servers remains. 
{{/columnate2}}

[some link](/some-menu)
```

Into this:

```
══════════════════════════════════════════════════════════════════════════════════════════
██████████████████████████████████████████████████████████████████████████████████████████
█   ██ █ ██▀ ▀██ █ ██  ▀██ ████   ███████  ▀██ █ ██ ████▀ ▀██▀  ███████  ▀██▀ ▀██▀  ██   █
█ ████ █ ██ █ ██   ██ █ ██ ████ █████████ █ ██ █ ██ ████ █ ██ █████████ █ ██ █ ██ █████ ██
█   ███ ███ ▄ ██   ██  ▄██ ████   ███████  ▄██   ██ ████ █ ██ █\███████  ▄██ █ ██  ▀███ ██
█ ████ █ ██ █ ██ █ ██ ████ ████ █████████ ████ █ ██ ████ █ ██ █ ███████ ████ █ ████ ███ ██
█   ██ █ ██ █ ██ █ ██ ████   ██   ███████ ████ █ ██   ██▄ ▄██▄  ███████ ████▄ ▄██  ▄███ ██
██████████████████████████████████████████████████████████████████████████████████████████
══════════════════════════════════════════════════════════════════════════════════════════

Posted: 2021-06-12
Updated: 2021-06-13
Author: someodd
Tags: foo, bar

This is an example phlog post.

Some text to replace someVariable

═════════════════════════════════════════════════════════════════════════════════════
█████████████████████████████████████████████████████████████████████████████████████
█   ██▀ ▀██ ████ █ ██ █ ██ ████▀ ▀██   ██   ███████   ██ █ ██▀ ▀██ █ ██  ▀██ ████   █
█ ████ █ ██ ████ █ ██   ██  ▀██ █ ███ ███ █████████ ████ █ ██ █ ██   ██ █ ██ ████ ███
█ ████ █ ██ ████ █ ██   ██ █ ██ ▄ ███ ███   ███████   ███ ███ ▄ ██   ██  ▄██ ████   █
█ ████ █ ██ ████ █ ██ █ ██ █ ██ █ ███ ███ █████████ ████ █ ██ █ ██ █ ██ ████ ████ ███
█   ██▄ ▄██   ██   ██ █ ██ █ ██ █ ███ ███   ███████   ██ █ ██ █ ██ █ ██ ████   ██   █
█████████████████████████████████████████████████████████████████████████████████████
═════════════════════════════════════════════════════════════════════════════════════

The  Gopher protocol  /ˈɡoʊfər/  is  a │ interface  is well-suited to computing
communications  protocol designed  for │ environments   that  rely  heavily  on
distributing,      searching,      and │ remote      text-oriented     computer
retrieving   documents   in   Internet │ terminals,  which were still common at
Protocol  networks. The  design of the │ the  time of its creation in 1991, and
Gopher  protocol and user interface is │ the   simplicity   of   its   protocol
menu-driven,    and    presented    an │ facilitated  a wide  variety of client
alternative  to the  World Wide Web in │ implementations.  More  recent  Gopher
its  early stages, but ultimately fell │ revisions  and graphical clients added
into   disfavor,   yielding   to   the │ support    for    multimedia.[citation
Hypertext Transfer Protocol (HTTP) The │ needed]  Gopher was  preferred by many
Gopher  ecosystem is often regarded as │ network administrators for using fewer
the effective predecessor of the World │ network     resources     than     Web
Wide Web.[1] The protocol was invented │ services.[3]   Gopher's   hierarchical
by  a team  led by Mark P. McCahill[2] │ structure  provided a platform for the
at  the University  of  Minnesota.  It │ first  large-scale electronic  library
offers   some  features  not  natively │ connections.[4] The Gopher protocol is
supported  by the  Web and  imposes  a │ still   in  use  by  enthusiasts,  and
much   stronger   hierarchy   on   the │ although  it has  been almost entirely
documents  it stores.  Its  text  menu │ supplanted   by  the   Web,  a   small

population    of   actively-maintained
servers remains.

0some link      /some-menu

bar
```
