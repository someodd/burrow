{-
  Implement YAML headers to give certain files more context. Right now it's
  currently used for tagging phlog posts.

  The tagging system simply collects all of the tags noticed throughout the
  site, then creates an index under /tags/sometag in the output, allowing you to
  view posts by tag.

  At some point I will have to delete the YAML header before other stuff gets
  parsed.
-}

module Tagging where


data FrontMatter = FrontMatter { tags = [Text] }

type Tag = Text

data TagIndexes = Map...Text Tag

{-
 Take in a list...
buildIndexes markdownParsed
