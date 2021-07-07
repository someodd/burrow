-- | A very lazily named common module where I am putting things to avoid
-- circular dependencies, out of laziness.
module Types (ContentType(..)) where


-- | Content type that the builder recognizes/finds useful in the build
-- process. This describes the kind of file it will be as an end product in the
-- gopherhole.
data ContentType = GopherFileType
                 -- ^ Parse to plaintext ASCII-art-style file.
                 | GopherMenuType
                 -- ^ Will not be parsed! Will simply be copied.
                 deriving (Show, Eq)
