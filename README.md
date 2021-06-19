# Burrow: Make Gopherholes

Burrow is a *static site generator,* but for gopherholes. A *gopherhole* is a place in *gopherspace* accessed through the [Gopher protocol](https://en.wikipedia.org/wiki/Gopher_%28protocol%29)).

There's an example gopherhole you can build with these command and then visit `localhost:7070` in `gopher`:

```
cabal run burrow -- --source example-gopherhole/ --destination built/ --spacecookie
cd built
spacecookie ../data/spacecookie.json
```

Written in Haskell. If you're looking for very similar software, with coincidentally the exact same name, please check out [James Tomasino's Burrow](https://github.com/jamestomasino/burrow).

## Note about GHC

Currently using GHC 8.10.4, because GHC 8.10.5 breaks DocTests and the latest
version of GHC which supports Haskell/GHC 2021 isn't supported by Cabal yet.

## Features

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

Use [Mustache](https://mustache.github.io/) for templating.

There are built in lambdas for your convenience:

  * `{{#columnate2}}this text will be justified and broken up like a newspaper{{/columnate2}}`

### Markdown

Commonmark parser is used.

### Phlogging features

Indexes will be generated as gophermaps for posts which are tagged and have all
the required front matter (like `published`, `title`, and `type: post`).

Atom feeds will be generated for each index (tag indexes, the main feed, and
tag summaries).

### Change a lot of the behavior through an INI

Be sure to look at `gopherhole.ini` in order to control and customize the builder!

## Other notes

This software has primarily been tested with
[spacecookie](https://github.com/sternenseemann/spacecookie) (Gopher server).
You may want to look at [my spacecookie Docker
repo](https://github.com/hyperrealgopher/docker-spacecookie). Currently, Burrow relies heavily on the `.gophermap` behavior outlined in these documents:

* [spacecookie.gophermap(5)](https://sternenseemann.github.io/spacecookie/spacecookie.json.5.html)
* [Bucktooth's gophermap tutorial](http://gopher.floodgap.com/gopher/gw.lite?=gopher.floodgap.com+70+302f6275636b2f6462726f7773653f666171757365203161)
* [Bucktooth](http://gopher.floodgap.com/gopher/gw.lite?=gopher.floodgap.com+70+312f6275636b).
* [pygopherd(8)](https://manpages.debian.org/unstable/pygopherd/pygopherd.8.en.html)
* [pygopherd](https://github.com/jgoerzen/pygopherd)

Other good things to read about and know:

* [mustache](https://mustache.github.io/mustache.5.html)
* [CommonMark](https://commonmark.org/): a great Markdown standard (used by GitHub, Reddit, Stack Overflow)
* You should know about [the Gopher protocol](https://en.wikipedia.org/wiki/Gopher_%28protocol%29)

### Font system

The font system is primarily used by the markdown parser for headings.

Headings are parsed in a way which creates fancy ASCII art headings. You can specify which ASCII art
font to use in `data/gopherhole.ini`.

The font spec is simple: on one line is a character you wish to represent, followed by the ASCII art for the character. Each line for the character must be of the same length. Each character in the font must be of the same number of lines. Separate character definitions with blank lines.

For more info please see `data/fonts/`.

## Example

Turn this:

```markdown
---
title: Example phlog post
published: 2021-06-12
updated: June 13th, 2021 at 10pm
author: Hyperreal Gopher
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
Author: Hyperreal Gopher
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

## Running tests

Run the doc tests:

`cabal exec cabal test`
