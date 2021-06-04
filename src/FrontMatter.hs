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
module FrontMatter (renderMainPhlogIndex, renderTagIndexes, getFrontMatter, FrontMatter) where

import Control.Monad.Reader
import Control.Arrow ((&&&))
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Dates.Parsing as DP
import System.FilePath (takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Data.Foldable (traverse_)
import Data.Maybe
import qualified Data.HashMap.Strict as HashMap
import Data.Attoparsec.ByteString ((<?>))
import Data.ByteString as ByteString hiding (head, isSuffixOf, take, intercalate, map, foldr, writeFile)
import Data.Frontmatter
import Data.Yaml (FromJSON, parseJSON, withObject, (.:?), (.:), decodeEither')
import Data.Text.Encoding         as T
import Data.Text as T hiding (head, isSuffixOf, take, map, foldr, intercalate)
import Data.List (sortOn, intercalate, isSuffixOf)

import Config (getConfig, getConfigValue, ConfigParser)
import Common (gophermapSuffix, textFileSuffix)


-- | FrontMatter that is particularly useful for Burrow.
data FrontMatter = FrontMatter
  { tags :: ![Text]-- what if space separated list of tags? TODO: for jekyll frontmatter spec
  , date :: Maybe Text
  , title :: Maybe Text
  } deriving (Show)

-- TODO: better document
-- | The parser used for creating `FrontMatter` types.
instance FromJSON FrontMatter where
  parseJSON = withObject "FrontMatter" $ \o -> FrontMatter <$> o .: "tags" <*> o .:? "date" <*> o .:? "title"

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


class PhlogIndex a where
  -- can derive these
  --createIndex :: PostPageMeta -> a
  --createRSS :: PostPageMeta -> String?
  -- writeRSS :: a -> IO ()

  -- | Create the data model...
  createIndex :: PostPageMeta -> a

  -- | Do all the work required to write a string of the data model to file...
  writeIndex :: a -> IO ()


-- The Int is the current year
newtype MainPhlogIndex = MainPhlogIndex (Reader Integer PostPageMeta)

instance PhlogIndex MainPhlogIndex where
  createIndex postPageMetaPairs = do
    MainPhlogIndex $ do
      currentYear <- ask
      pure $ makePhlogIndex postPageMetaPairs currentYear
   where
    -- TODO: actually parse into a nice thing
    -- | An index (gophermap) of all the phlog posts! Just sorts all the
    -- filepath/frontmatter pairs.
    makePhlogIndex :: PostPageMeta -> Integer -> PostPageMeta
    makePhlogIndex meta defaultYear =
      sortOn (\y -> snd y >>= \fm -> dateStringToEpoch defaultYear <$> date fm) meta

  writeIndex (MainPhlogIndex mainPhlogIndex) = do
    -- TODO: list main tag index
    -- TODO: 
    --  createDirectoryIfMissing True (takeDirectory mainTagIndexPath)
    -- FIXME: directories need to be defined in config
    -- | Create the main phlog index which includes all the posts sorted by date.
    currentYear <- getCurrentYear
    configParser <- getConfig
    buildPath <- getConfigValue configParser "general" "buildPath"
    phlogPath <- getConfigValue configParser "phlog" "phlogPath"
    tagPath <- getConfigValue configParser "phlog" "tagPath"
    let outputPath = buildPath ++ "/" ++ phlogPath ++ "/.gophermap"
        phlogIndex = runReader mainPhlogIndex currentYear
    writeFile outputPath $ makePhlogIndexPage phlogIndex tagPath
   where
    makePhlogIndexPage :: PostPageMeta -> FilePath -> String
    makePhlogIndexPage meta tagIndexPath =
      let viewByTagsEntry = show $ MenuLink "1" "view by tags" tagIndexPath Nothing Nothing
          allThePosts = "all phlog posts\n" ++ (intercalate "\n" $ map (makeLocalLink) meta)
      in viewByTagsEntry ++ "\n" ++ allThePosts


renderMainPhlogIndex :: PostPageMeta -> IO ()
renderMainPhlogIndex postPageMetaPairs = do
  let mainPhlogIndex = createIndex postPageMetaPairs :: MainPhlogIndex
  writeIndex mainPhlogIndex


-- | This is for the default date while interpreting FrontMatter dates.
getCurrentYear :: IO Integer
getCurrentYear = (\(y,_,_) -> y) <$> (getCurrentTime >>= return . toGregorian . utctDay) 


newtype SpecificTagIndex = SpecificTagIndex (HashMap.HashMap T.Text [FilePath])

instance PhlogIndex SpecificTagIndex where
  createIndex postPageMetaPairs =
    SpecificTagIndex $ HashMap.fromListWith (++) $ sorted
   where
    unsorted :: [(T.Text, [(FilePath, Maybe T.Text)])]
    unsorted = [(tag, [(filePath, date fm)]) | (filePath, Just fm) <- postPageMetaPairs, tag <- tags fm]

    -- needs to be sorted by date, descending
    sorted :: [(T.Text, [FilePath])]
    sorted = map (\(tag, fileDateList) -> (tag, map fst $ sortOn snd fileDateList)) unsorted

  -- FIXME: wayyyyy too big and complicated!!!
  writeIndex (SpecificTagIndex specificTagIndex) = do
    -- FIXME: tag index needs to use frontmatter again not just paths?
    -- TODO/FIXME: it's called write but it's really doing heavy lifting for parsing list into files?
    -- TODO: sort by pubdate?
    -- FIXME: TagIndex should be more robust and not just store the file path but more info as well. maybe frontmatter should be modified to include the filepath! although it could just include more metadata like pubdate etc etc to make it more useful...
    -- | Write out tag indexes to a directory...
    -- get paths from config
    configParser <- getConfig
    buildPath <- getConfigValue configParser "general" "buildPath"
    tagPath <- getConfigValue configParser "phlog" "tagPath"
    let mainTagIndexPath = buildPath ++ "/" ++ tagPath ++ "/.gophermap"

    -- write out the main tag index
    createDirectoryIfMissing True (takeDirectory mainTagIndexPath)
    writeFile mainTagIndexPath (makeMainTagIndex tagPath)

    traverse_ (writeTagIndexF configParser) allTags
   where
    writeTagIndexF :: ConfigParser -> T.Text -> IO ()
    writeTagIndexF configParser tag = do
      tagIndexPath <- getConfigValue configParser "phlog" "tagPath"
      buildPath <- getConfigValue configParser "general" "buildPath"
      let outputPath = buildPath ++ "/" ++ tagIndexPath ++ "/" ++ (T.unpack tag) ++ "/.gophermap"
          outputDirectoryPath = takeDirectory outputPath
      createDirectoryIfMissing True outputDirectoryPath
      writeFile outputPath (tagIndexContents tag)

    allTags :: [T.Text]
    allTags = HashMap.keys specificTagIndex

    -- | Write the index of tag indexes! This makes it so the user can see all the tags and
    -- see five example posts (should make this sorted by date...) should include instead of
    -- filepath include a bunch of data necessary for building the indexes.
    makeMainTagIndex :: FilePath -> String
    makeMainTagIndex tagIndexDirectory =
      -- fromjust is bad
      -- FIXME: this kleisli function is a hack because no fm was included in tag index at the moment
      let viewAllPostsEntry tag = show $ MenuLink "1" ("View all posts tagged " ++ (T.unpack tag)) (tagIndexDirectory ++ (T.unpack tag)) Nothing Nothing
          threeSummary tag = intercalate "\n"
            [ (T.unpack tag)
            , (intercalate "\n" $ map (makeLocalLink . (id &&& const Nothing)) $ take 3 (fromJust $ HashMap.lookup tag specificTagIndex))
            , viewAllPostsEntry tag
            ]
      in intercalate "\n\n" $ map threeSummary allTags

    tagIndexContents :: T.Text -> String
    tagIndexContents tag =
      -- fromjust bad! FIXME
      (T.unpack tag) ++ "\n\n" ++ (intercalate "\n" $ map (makeLocalLink . (id &&& const Nothing)) $ fromJust $ HashMap.lookup tag specificTagIndex)


-- | Render the tag indexes from a collection of file paths and their
-- associated `FrontMatter`, which contains the tags for that file.
--
-- The tags are written out to the supplied `FilePath`.
renderTagIndexes :: PostPageMeta ->  IO ()
renderTagIndexes filePathFrontMatter = do
  let specificTagIndex = createIndex filePathFrontMatter :: SpecificTagIndex
  writeIndex specificTagIndex


-- TODO: this should be plural... maybe it should be [PostPageMeta]
-- TODO: better names? filefrontmatterpairs?
-- | Pairs of filepaths associated with their frontmatter. Useful for posts and pages.
type PostPageMeta = [(FilePath, Maybe FrontMatter)]

{-
-- could have [(FIlePath, Maybe FrontMatter)] have many types/type synonyms?
-- like AllTagsIndex and SpecificTagIndex
class PhlogIndex a where
  writeIndex :: String
  writeIndex .. = ...
-}


dateStringToEpoch :: Integer -> T.Text -> DP.DateTime
dateStringToEpoch defaultYear dateText = head $ DP.extractDateTimesY (fromIntegral defaultYear :: Int) (T.unpack dateText)


-- | A useful tool in making phlog indexes: create a nice link for the menu
-- using a supplied pair from PostPageMeta.
makeLocalLink :: (FilePath, Maybe FrontMatter) -> String
makeLocalLink (path, maybeFrontMatter)
  | gophermapSuffix `isSuffixOf` path = restOfLink "1"
  | textFileSuffix `isSuffixOf` path = restOfLink "0"
  -- Otherwise we are very lazily just going to assume it's a text file. This
  -- will be improved in the future.
  | otherwise = restOfLink "1"
 where
  restOfLink :: String -> String
  restOfLink itemType = show $ MenuLink itemType label path Nothing Nothing

  label :: String
  label = fromMaybe path (maybeFrontMatter >>= \fm -> T.unpack <$> title fm)



data MenuLink = MenuLink
  { linkType :: String
  , displayString :: String
  , selector :: String
  , server :: Maybe String
  , port :: Maybe Int }

-- | Make a spacecookie spec .gophermap link.
--
-- gopherfiletypeNAME\t [SELECTOR [\tSERVER [\tPORT]]]
--
-- https://sternenseemann.github.io/spacecookie/spacecookie.gophermap.5.html
instance Show MenuLink where
  show ml =
    let firstPart =
          intercalate "\t"
            [ (linkType ml) ++ (displayString ml)
            , "/" ++ (selector ml)
            ]
        secondPart =
          fromMaybe "" (server ml >>= \x -> port ml >>= \y -> Just $ "\t" ++ x ++ "\t" ++ (show y))
    in firstPart ++ secondPart



