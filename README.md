# Burrow: Make Gopherholes

**WARNING:** Currently this is a proof-of-concept. Alpha. It is not fit for
production use. It will need to be cleaned and documented and the output will
change a bit to be more consistent. I uploaded it to GitHub just to show off
this proof-of-concept since it's almost done.

It's like a static site generator, but for gopherholes.  A gopherhole is a
place in gopherspace reached via [the Gopher protocol](https://en.wikipedia.org/wiki/Gopher_%28protocol%29).

Creeate a gopherhole from Markdown and [Mustache](https://mustache.github.io/)
files. Menus are created from `*.md.mustache` files. Text files are created
from `*.txt.mustache` files.

Example usage (I even included an example `test-gopherhole/` source directory in this repo):

```
cabal run burrow -- --source test-gopherhole/ --destination built/ --spacecookie
```

## Spacecookie support

This software has primarily been tested with
[spacecookie](https://github.com/sternenseemann/spacecookie) (Gopher server).
You may want to look at [my spacecookie Docker
repo](https://github.com/hyperrealgopher/docker-spacecookie).

To generate `.gophermaps` from `index.md.mustache` files use the
`--spacecookie` flag. Although untested, this should also work for
[pygopherd](https://github.com/jgoerzen/pygopherd) and
[Bucktooth](http://gopher.floodgap.com/gopher/gw.lite?=gopher.floodgap.com+70+312f6275636b).

See also:

* [spacecookie-gophermap(5)](https://github.com/sternenseemann/spacecookie/blob/master/docs/man/spacecookie-gophermap.5)
* [Bucktooth's gophermap tutorial](http://gopher.floodgap.com/gopher/gw.lite?=gopher.floodgap.com+70+302f6275636b2f6462726f7773653f666171757365203161)
* [pygopherd(8)](https://manpages.debian.org/unstable/pygopherd/pygopherd.8.en.html)

## Notes

  * First the Mustache is parsed, then the Markdown
  * Put your Mustache templates in `templates/` so you can include them in Mustache files.
  * Take a look at `example-gopherhole-source/` and `templates/` for some tempalate examples.
    I use `templates/menu.md.mustache` to render all `*.md.mustache` files. This makes it easier
    to have includes for stuff like a header and footer without needing to use the include in every
    menu (`*.md.mustache`) you wish to parse

### Planned features

  * Configuration script for changing what the parser changes markdown things
    to like bold, headings, etc.
  * More text formatting features (as Mustache lambdas) like creating columns
  * Better heading ASCII art font support
  * Phlogging tools!

## The markdown parser

### Header fonts

Markdown headers will be parsed to fancy ASCII art font...

## Mustache

### Partial system

Mustache supports "partials." This is how they work:

```
{{> somefile }}
```

I guess? What about search path idk...

#### File-extension-based templating

You can create various types of templates in `mustache/extensions`...

#### Include-style templating

You can create partials to use in templates with  `{{>
somefile.txt.mustache}}`... They should be in `mustache/includes`.

#### Mustache lambdas

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
