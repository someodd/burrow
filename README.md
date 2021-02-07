# Burrow: Make Gopherholes

It's like a *static site generator,* but for gopherholes (places in gopherspace reached via [the Gopher protocol](https://en.wikipedia.org/wiki/Gopher_%28protocol%29)).


Use [Mustache](https://mustache.github.io/) templates of Markdown (specifically Commonmark) to create gophermaps/menus and text files. Through the use of Mustache lambdas, partial templates, Markdown, and file extensions you can turn this file (`special.headfoot.partial.menu.md.mustache`):

```
## Columnate example

{{#columnate2}}
The Gopher protocol /ˈɡoʊfər/ is a communications protocol designed for distributing, searching, and retrieving documents in Internet Protocol networks. The design of the Gopher protocol and user interface is menu-driven, and presented an alternative to the World Wide Web in its early stages, but ultimately fell into disfavor, yielding to the Hypertext Transfer Protocol (HTTP) The Gopher ecosystem is often regarded as the effective predecessor of the World Wide Web.[1]
The protocol was invented by a team led by Mark P. McCahill[2] at the University of Minnesota. It offers some features not natively supported by the Web and imposes a much stronger hierarchy on the documents it stores. Its text menu interface is well-suited to computing environments that rely heavily on remote text-oriented computer terminals, which were still common at the time of its creation in 1991, and the simplicity of its protocol facilitated a wide variety of client implementations. More recent Gopher revisions and graphical clients added support for multimedia.[citation needed] Gopher was preferred by many network administrators for using fewer network resources than Web services.[3]
Gopher's hierarchical structure provided a platform for the first large-scale electronic library connections.[4] The Gopher protocol is still in use by enthusiasts, and although it has been almost entirely supplanted by the Web, a small population of actively-maintained servers remains. 
{{/columnate2}}

[some link](/some-menu)
```

Into this:

```
HEAD

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

FOOT
```

The above demonstrates one of the fancy text formatting Mustache lambdas I've made (`{{columnate2}}`), the header/ASCII art font system, Markdown-to-gophermap parsing, and the partials/templating system (including a parent template by specifying a special extension, to put the content between a `HEAD` and `FOOT`, from the `templates/headfoot.menu.md.mustache` file).

Demo `templates/` and an example `test-gopherhole/` is at your disposal. Try building and looking at the files! Files without the Burrow extensions are simply copied instead of parsed. The directory structure is preserved on build.

## Quickstart

```
cabal run burrow -- --source test-gopherhole/ --destination built/ --spacecookie
```

## Things to know/good resources

Things you will need to know about in order to make your gopherhole:

  * [mustache](https://mustache.github.io/mustache.5.html)
  * [CommonMark](https://commonmark.org/): a great Markdown standard (used by GitHub, Reddit, Stack Overflow)
  * You should know about [the Gopher protocol](https://en.wikipedia.org/wiki/Gopher_%28protocol%29)

## Spacecookie support

I plan on making this the default behavior...

This software has primarily been tested with
[spacecookie](https://github.com/sternenseemann/spacecookie) (Gopher server).
You may want to look at [my spacecookie Docker
repo](https://github.com/hyperrealgopher/docker-spacecookie).

To generate `.gophermaps` from `index.menu.md.mustache` files use the
`--spacecookie` flag. Although untested, this should also work for
[pygopherd](https://github.com/jgoerzen/pygopherd) and
[Bucktooth](http://gopher.floodgap.com/gopher/gw.lite?=gopher.floodgap.com+70+312f6275636b).

See also:

* [spacecookie-gophermap(5)](https://github.com/sternenseemann/spacecookie/blob/master/docs/man/spacecookie-gophermap.5)
* [Bucktooth's gophermap tutorial](http://gopher.floodgap.com/gopher/gw.lite?=gopher.floodgap.com+70+302f6275636b2f6462726f7773653f666171757365203161)
* [pygopherd(8)](https://manpages.debian.org/unstable/pygopherd/pygopherd.8.en.html)

## Procedure

First the [Mustache](https://mustache.github.io/) substitutions are ran/the file is assembled. Then once
the substitutions are ran the Markdown parser is ran on them, parsing out as
either a text file or a gophermap/menu (either intended for gopherspace).

The directory structure is preserved. To use the parser you must use these file
extensions:

  * `*.text.md.mustache`: The file will first have Mustache substitutions ran over it,
    then the Burrow Markdown parser for text files in gopherspace will be ran on the result.
  * `*.menu.md.mustache`: The file will first have Mustache substitutions ran over it,
    then the Burrow Markdown parser for gophermaps/menus will be ran on the result.

Files will be output to the output directory which is used as your gopherhole contents.

### Partial templates

You can manually specify/include partials from `templates/` like this:

```
{{> header.md.mustache}}
```

There's another partial templating system that Burrow supports. A *partial
template* allows you to put a file *inside another file* from `templates/`.
This works using Mustache templates and using a special file extension.

You can also use templates using the *partials* systems. To use a partial
template you just need to give your file an extension like
`somefile.phlog_index.partial.menu.md.mustache`. To break this down:

  * `somefile`: The name of the file.
  * `phlog_index.partial`: Indicates that we want to use the `phlog_index` partial template.
    This means the file will be put inside the menu partial template.
  * The menu partial template will match the type of the file, in this case `menu`
    (gophermap/menu), so the corresponding file will be `templates/phlog_index.menu.md.mustache`.

### Example

Here may be the example `somefile.phlog_index.partial.menu.md.mustache`:

```
test
```

And here's an example `phlog_index.menu.md.mustache`:

```
{{! This is the base template for phlog listings! }}

BEFORE

{{> partial}}

AFTER
```

Here's what Burrow will output to your output directory `phlog_index.menu.md`:

```
BEFORE

test

AFTER
```

## Font system

Headings are parsed in a way which creates fancy ASCII art headings. You can specify which ASCII art
font to use in `data/gopherhole.ini`.

The font spec is simple: on one line is a character you wish to represent, followed by the ASCII art for the character. Each line for the character must be of the same length. Each character in the font must be of the same number of lines. Separate character definitions with blank lines.

For more info please see `data/fonts/`.

## Using Mustache lambdas

You have some Mustache lambdas at your disposal. Use them like this `justify` function, which wil create a justified block of text (each line of text will have the words spaced to each line is of equal length):

```
{{#justify}}
This text will be justify-aligned.  Lorem ipsum dolor sit amet, consectetur
adipiscing elit. Fusce vel metus in justo sodales congue. Cras mi nulla,
bibendum eget dui ut, ultrices porta tellus. Morbi rutrum nulla in est tempus
ornare. Sed sed mi nulla. Phasellus mollis, mi vitae mollis venenatis, magna
nunc tristique enim, vitae auctor augue sapien quis turpis. Suspendisse maximus
erat nulla, posuere rutrum dui tempor sed. Nunc nunc tortor, egestas non
eleifend feugiat, scelerisque ut purus. Suspendisse ac lorem iaculis, malesuada
turpis quis, facilisis nisl. Donec dignissim condimentum semper. Interdum et
malesuada fames ac ante ipsum primis in faucibus. Pellentesque egestas magna
vitae rutrum pharetra. Nulla mattis tristique facilisis. Proin efficitur
lacinia commodo.
{{/justify}}
```

Other Mustache lambdas:

  * columnate2: break a block of text into a maximum of two columns
