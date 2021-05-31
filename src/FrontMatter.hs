-- | Implements YAML headers of plaintext files in order to specify
-- metadata, like tagging blog posts. This is modelled after Jekyll's
-- FrontMatter: https://jekyllrb.com/docs/front-matter/
--
-- Here's an example of tagging a blog post:
--
--   ---
--   tags: [foo, bar]
--   ---
--   blog content here
--
-- ...
{-# LANGUAGE OverloadedStrings          #-}
module FrontMatter (renderTagIndexes, getFrontMatter, FrontMatter) where

import System.FilePath (takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Data.Foldable (traverse_)
import Data.Maybe
import qualified Data.HashMap.Strict as HashMap
import Data.Attoparsec.ByteString ((<?>))
import Data.ByteString as ByteString hiding (isSuffixOf, take, intercalate, map, foldr, writeFile)
import Data.Frontmatter
import Data.Yaml (FromJSON, parseJSON, withObject, (.:), decodeEither')
import Data.Text.Encoding         as T
import Data.Text as T hiding (isSuffixOf, take, map, foldr, intercalate)
import Data.List (intercalate, isSuffixOf)
import Common (gophermapSuffix, textFileSuffix)


-- | FrontMatter that is particularly useful for Burrow.
data FrontMatter = FrontMatter
  { tags :: ![Text]-- what if space separated list of tags? TODO: for jekyll frontmatter spec
  } deriving (Show)

-- TODO: better document
-- | The parser used for creating `FrontMatter` types.
instance FromJSON FrontMatter where
  parseJSON = withObject "FrontMatter" $ \o -> FrontMatter <$> o .: "tags"

-- TODO: document
frontmatterBurrow :: Parser FrontMatter
frontmatterBurrow = frontmatterYaml' <?> "frontmatterBurrow"
  where
    frontmatterYaml' = do
        f <- frontmatter
        case decodeEither' f of
            Left e -> error (show e)
            --Left e -> fail (show e)
            Right v -> return v

-- TODO: document
-- FIXME: doesn't need filepath
-- FIXME: associate filepath with the value and stuff?
getFrontMatter :: FilePath -> T.Text -> (Maybe FrontMatter, T.Text)
getFrontMatter _ text = do
  let bsText = T.encodeUtf8 text
      result = parse frontmatterBurrow bsText
  case result of
      Done ri fm -> (Just fm, T.decodeUtf8 ri) -- aeson object in th efrontmatter
      Partial f -> case (f ByteString.empty) of
                     Done ri fm -> (Just fm, T.decodeUtf8 ri)
                     Partial _ -> error "wtf"
                     Fail _ _ errorMsg -> error errorMsg
        -- aeson object in the frontmatter (the explicit
        -- type required because the parser will return
        -- anything with a FromJSON
        --  and then the rest of the doc
      Fail _ _ _ -> (Nothing, text)


type TagIndex = HashMap.HashMap T.Text [FilePath]

makeTagIndex :: [(FilePath, Maybe FrontMatter)] -> TagIndex
makeTagIndex pairs =
  HashMap.fromListWith (++) $ [(tag, [filePath]) | (filePath, Just fm) <- pairs, tag <- tags fm]

-- TODO: sort by pubdate?
-- FIXME: TagIndex should be more robust and not just store the file path but more info as well. maybe frontmatter should be modified to include the filepath! although it could just include more metadata like pubdate etc etc to make it more useful...
-- | Write out tag indexes to a directory...
writeTagIndex :: FilePath -> TagIndex -> IO ()
writeTagIndex outputDirectory tagIndex = do
  -- write out the main tag index
  let mainTagIndexPath = outputDirectory ++ "/" ++ mainTagIndexLocation
  createDirectoryIfMissing True (takeDirectory mainTagIndexPath)
  writeFile mainTagIndexPath makeMainTagIndex

  traverse_ writeTagIndexF allTags
 where
  writeTagIndexF tag = do
    let outputPath = outputDirectory ++ "/" ++ (tagIndexLocation tag)
        outputDirectoryPath = takeDirectory outputPath
    createDirectoryIfMissing True outputDirectoryPath
    writeFile outputPath (tagIndexContents tag)

  makeLocalLink :: FilePath -> String
  makeLocalLink path
    | gophermapSuffix `isSuffixOf` path = "0" ++ path ++ "\t" ++ path
    | textFileSuffix `isSuffixOf` path = "1" ++ path ++ "\t" ++ path
    | otherwise = error "should be impossible"

  allTags :: [T.Text]
  allTags = HashMap.keys tagIndex

   -- | Where the index of tags along with a few example posts will be stored...
  mainTagIndexLocation = "tag_list"
  -- | Write the index of tag indexes! This makes it so the user can see all the tags and
  -- see five example posts (should make this sorted by date...) should include instead of
  -- filepath include a bunch of data necessary for building the indexes.
  makeMainTagIndex =
        -- fromjust is bad
    let threeSummary tag = (T.unpack tag) ++ "\n\n" ++ (intercalate "\n" $ map makeLocalLink $ take 3 (fromJust $ HashMap.lookup tag tagIndex))
    in intercalate "\n\n" $ map threeSummary allTags

  tagIndexLocation :: T.Text -> FilePath
  tagIndexLocation tag = "tags/" ++ (T.unpack tag)

  tagIndexContents :: T.Text -> String
  tagIndexContents tag =
    -- fromjust bad!
    (T.unpack tag) ++ "\n\n" ++ (intercalate "\n" . map makeLocalLink . fromJust $ HashMap.lookup tag tagIndex)


-- | Render the tag indexes from a collection of file paths and their
-- associated `FrontMatter`, which contains the tags for that file.
--
-- The tags are written out to the supplied `FilePath`.
renderTagIndexes :: FilePath -> [(FilePath, Maybe FrontMatter)] ->  IO ()
renderTagIndexes destDir filePathFrontMatter =
  let tagIndex = makeTagIndex filePathFrontMatter
  in writeTagIndex destDir tagIndex
