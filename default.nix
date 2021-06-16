{ mkDerivation, aeson, attoparsec, base, bytestring, commonmark
, ConfigFile, containers, data-default, directory, errata, filepath
, filepattern, frontmatter, fuzzy-dates, hashable, hourglass, lib
, mtl, mustache, neat-interpolation, optparse-applicative, parsec
, raw-strings-qq, split, text, time, unordered-containers, vector
, word-wrap, xml-conduit, xml-conduit-writer, yaml
}:
mkDerivation {
  pname = "burrow";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring commonmark ConfigFile containers
    data-default directory errata filepath filepattern frontmatter
    fuzzy-dates hashable hourglass mtl mustache neat-interpolation
    optparse-applicative parsec raw-strings-qq split text time
    unordered-containers vector word-wrap xml-conduit
    xml-conduit-writer yaml
  ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring commonmark ConfigFile containers
    data-default directory errata filepath filepattern frontmatter
    fuzzy-dates hashable hourglass mtl mustache neat-interpolation
    optparse-applicative parsec raw-strings-qq split text time
    unordered-containers vector word-wrap xml-conduit
    xml-conduit-writer yaml
  ];
  homepage = "https://github.com/hyperrealgopher/burrow";
  description = "Build gopherholes";
  license = lib.licenses.gpl3Only;
}
