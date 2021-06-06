{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
module Phlog
  (
  -- * A type used for producing the models necesary for phlog indexes.
    PostPageMeta(..)
  -- * Produce different kinds of indexes for the phlog.
  -- $phlogIndexes
  , renderMainPhlogIndex
  , renderTagIndexes
  -- * Re-exports to make handling phlogs easier.
  , FrontMatter
  , getFrontMatter
  ) where

import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import qualified Data.Dates.Parsing as DP
import System.FilePath (takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.HashMap.Strict as HashMap
import Control.Arrow ((&&&))
import Control.Monad.Reader (runReader, Reader, ask)
import Data.List (sortOn, intercalate, isSuffixOf)
import qualified Data.Text as T

import Config (getConfig, getConfigValue, ConfigParser)
import Common (gophermapSuffix, textFileSuffix)
import FrontMatter (FrontMatter(..), getFrontMatter)


-- | Represents different kinds of phlog indexes and the tools required to do
-- things with them.
class PhlogIndex a b | b -> a where
  -- can derive these
  --createRSS :: PostPageMeta -> String?
  -- writeRSS :: a -> IO ()

  -- | Create the data model of the phlog index in question.
  createIndexModel :: a -> b

  -- NOTE: isn't this sloppy? It does all the work of transforming the data.
  -- Weird set of responsibilities.
  -- | Render the phlog index model file in order to be viewed as a gophermap
  -- in gopherspace.  This includes creating the string/file contents from the
  -- supplied model, which is then written to file.
  renderIndexGophermap :: b -> IO ()


-- The Int is the current year
newtype MainPhlogIndex = MainPhlogIndex (Reader Integer PostPageMeta)

instance PhlogIndex PostPageMeta MainPhlogIndex where
  createIndexModel postPageMetaPairs = do
    MainPhlogIndex $ do
      currentYear <- ask
      pure $ makePhlogIndex postPageMetaPairs currentYear
   where
    -- TODO: actually parse into a nice thing
    -- | An index (gophermap) of all the phlog posts! Just sorts all the
    -- filepath/frontmatter pairs.
    makePhlogIndex :: PostPageMeta -> Integer -> PostPageMeta
    makePhlogIndex (PostPageMeta meta) defaultYear =
      PostPageMeta $ sortOn (\y -> snd y >>= \fm -> dateStringToDateTime defaultYear <$> date fm) meta

  renderIndexGophermap (MainPhlogIndex mainPhlogIndex) = do
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
    makePhlogIndexPage (PostPageMeta meta) tagIndexPath =
      let viewByTagsEntry = show $ MenuLink "1" "view by tags" tagIndexPath Nothing Nothing
          allThePosts = "all phlog posts\n" ++ (intercalate "\n" $ map (makeLocalLink) meta)
      in viewByTagsEntry ++ "\n" ++ allThePosts


renderMainPhlogIndex :: PostPageMeta -> IO ()
renderMainPhlogIndex postPageMetaPairs = do
  let mainPhlogIndex = createIndexModel postPageMetaPairs :: MainPhlogIndex
  renderIndexGophermap mainPhlogIndex


-- | This is for the default date while interpreting FrontMatter dates.
getCurrentYear :: IO Integer
getCurrentYear = (\(y,_,_) -> y) <$> (getCurrentTime >>= return . toGregorian . utctDay) 


type Tag = T.Text


-- FIXME TODO
-- in the future also should look more like a hashmap of tag to [(filepath, frontmatter)]
-- in fact passing the sorted tag index could be something handy to have as reader in both instances
-- for efficiency
newtype MainTagIndex = MainTagIndex (HashMap.HashMap Tag [(FilePath, FrontMatter)])

-- FIXME/TODO: there's a TON of overlap between this and the other tag indexes so write some helper functions
-- FIXME/TODO
-- | The phlog index which lists all the tags and the latest posts for each tag.
instance PhlogIndex PostPageMeta MainTagIndex where
  -- NOTE: due to PhlogIndex SpecificTagIndex being similar, this results in much overlap/recalculation...
  createIndexModel (PostPageMeta postPageMetaPairs) =
    MainTagIndex $ HashMap.map (take postsPerTag) $ HashMap.fromListWith (++) result
   where
    -- TODO/FIXME: make into config value... will have to use reader?
    postsPerTag :: Int
    postsPerTag = 3

    result :: [ (Tag, [(FilePath, FrontMatter)]) ]
    result =
      let defaultYear = 2021-- FIXME
          sorted = sortOn (\y -> snd y >>= \fm -> dateStringToDateTime defaultYear <$> date fm) postPageMetaPairs
      -- FIXME: this won't work here! (take)
      in [(tag, take postsPerTag [(filePath, fm)]) | (filePath, Just fm) <- sorted, tag <- tags fm]-- FIXME: what if no tags? that should error right and be just fine?

  renderIndexGophermap (MainTagIndex mainTagIndex) = do
    -- get paths from config
    configParser <- getConfig
    buildPath <- getConfigValue configParser "general" "buildPath"
    tagPath <- getConfigValue configParser "phlog" "tagPath"
    let mainTagIndexPath = buildPath ++ "/" ++ tagPath ++ "/.gophermap"

    -- write out the main tag index
    createDirectoryIfMissing True (takeDirectory mainTagIndexPath)
    writeFile mainTagIndexPath (makeMainTagIndex tagPath)
   where
    allTags :: [T.Text]
    allTags = HashMap.keys mainTagIndex

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
            -- FIXME: use of fromJust here is bad.
            , intercalate "\n" $ map makeLocalLink $ map (\(x,y) -> (x, Just y)) $ fromJust $ HashMap.lookup tag mainTagIndex
            , viewAllPostsEntry tag
            ]
      in intercalate "\n\n" $ map threeSummary allTags


newtype SpecificTagIndexes = SpecificTagIndexes (HashMap.HashMap Tag [(FilePath, FrontMatter)])

-- FIXME, TODO: break down into specific tag indexes? but if you do that you'll have to accept the tag as an argument as reader or something?
-- FIXME: but you just want to reference a specific tag? or is this going to handle literally ALL of the individual tag indexes? rename to SpecificTagIndexes
-- FIXME: this is actually handling too much. It's doing the main tag index *and*
-- the individual tag indexes!
-- | The tag indexes for each tag (many files).
instance PhlogIndex PostPageMeta SpecificTagIndexes where
  createIndexModel (PostPageMeta postPageMetaPairs) =
    SpecificTagIndexes $ HashMap.fromListWith (++) result
   where
    result :: [ (Tag, [(FilePath, FrontMatter)]) ]
    result =
      let defaultYear = 2021-- FIXME
          sorted = sortOn (\y -> snd y >>= \fm -> dateStringToDateTime defaultYear <$> date fm) postPageMetaPairs
      in [(tag, [(filePath, fm)]) | (filePath, Just fm) <- sorted, tag <- tags fm]-- FIXME: what if no tags? that should error right and be just fine?

  renderIndexGophermap (SpecificTagIndexes specificTagIndexes) = do
    configParser <- getConfig
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
    allTags = HashMap.keys specificTagIndexes

    tagIndexContents :: T.Text -> String
    tagIndexContents tag =
      -- fromjust bad! FIXME
      (T.unpack tag) ++ "\n\n" ++ (intercalate "\n" $ map (makeLocalLink . (fst &&& Just . snd)) $ fromJust $ HashMap.lookup tag specificTagIndexes)


-- | Render the tag indexes from a collection of file paths and their
-- associated `FrontMatter`, which contains the tags for that file.
--
-- The tags are written out to the supplied `FilePath`.
renderTagIndexes :: PostPageMeta ->  IO ()
renderTagIndexes filePathFrontMatter = do
  let specificTagIndexes = createIndexModel filePathFrontMatter :: SpecificTagIndexes
  renderIndexGophermap specificTagIndexes
  let mainTagIndex = createIndexModel filePathFrontMatter :: MainTagIndex
  renderIndexGophermap mainTagIndex


-- TODO: this should be plural... maybe it should be [PostPageMeta]
-- TODO: better names? filefrontmatterpairs?
-- | Pairs of filepaths associated with their frontmatter. Useful for posts and pages.
newtype PostPageMeta = PostPageMeta [(FilePath, Maybe FrontMatter)]


-- FIXME: could error out (usage of `head`)
-- | Fuzzy match a date time string (for a `FrontMatter` date/time definition).
--
-- `extractDateTimesY` does all the heavy lifting.
--
-- >>> dateStringToDateTime (2021 :: Integer) (T.pack "july 29")
-- DateTime {dtDate = Date {dateYear = 2021, dateMonth = July, dateDay = 29}, dtTime = TimeOfDay {todHour = 0h, todMin = 0m, todSec = 0s, todNSec = 0ns}}
-- >>> dateStringToDateTime (2021 :: Integer) (T.pack "2021-06-20T04:30")
-- DateTime {dtDate = Date {dateYear = 2021, dateMonth = June, dateDay = 20}, dtTime = TimeOfDay {todHour = 0h, todMin = 0m, todSec = 0s, todNSec = 0ns}}
dateStringToDateTime :: Integer -> T.Text -> DP.DateTime
dateStringToDateTime defaultYear dateText = head $ DP.extractDateTimesY (fromIntegral defaultYear :: Int) (T.unpack dateText)


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


-- TODO: Maybe belongs in common.
-- | Representation of a gophermap menu entry, specifically links and not info items.
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

{- $phlogIndexes

A phlog index is like blog indexes. There are different ways to index the posts to a phog.
There's the main index which simply is a list of *all* of the blog posts in order of date.
There's also individual tag indexes, which lists posts belonging to a specific tag. There's
also an index of all the tags, showing a few of the msot recent posts belonging to those
tags. The pattern here is that there's different ways of listing the meta/information
belonging to phlog posts, in a way that's presentable and easily perused by the user.

These specific kinds of indexes are the type that gets written to file to be browsed
in gopher. These same indexes also have associated RSS feeds.
-}
