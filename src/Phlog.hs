{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
module Phlog
  (
  -- * Produce different kinds of indexes for the phlog.
  -- $phlogIndexes
    renderMainPhlogIndex
  , renderTagIndexes
  , PostPageMeta(..)
  -- * Re-exports to make handling phlogs easier.
  , FrontMatter
  , getFrontMatter
  ) where

--import Data.Functor.Identity (Identity(..))
import Data.Foldable (toList)
import Data.Hashable (Hashable)
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

import Config (getConfig, getConfigValue)
import Common (gophermapSuffix, textFileSuffix)
import FrontMatter (FrontMatter(..), getFrontMatter)


-- | Represents different kinds of phlog indexes and the tools required to do
-- things with them.
--
-- Type `a` is what will create the model (type `b`) used in creating the
-- gophermap as well as the Atom feed.
class PhlogIndex a b | b -> a where
  -- TODO: renderAtom :: a -> IO ()

  -- | Sort the index model.
  sortIndexModel :: b -> b

  -- | Create the data model of the phlog index in question.
  createIndexModel' :: a -> b
  createIndexModel :: a -> b
  createIndexModel = sortIndexModel . createIndexModel'

  -- | Render the phlog index model file in order to be viewed as a gophermap
  -- in gopherspace.  This includes creating the string/file contents from the
  -- supplied model, which is then written to file.
  renderIndexGophermap :: b -> IO ()


-- The Int is the current year
newtype MainPhlogIndex = MainPhlogIndex (Reader Integer PostPageMeta)

instance PhlogIndex PostPageMeta MainPhlogIndex where
  sortIndexModel (MainPhlogIndex readerIntPostPageMeta) = do
      MainPhlogIndex $ do
        currentYear <- ask
        let sorter (PostPageMeta pairs) = PostPageMeta $ sortOn (\y -> snd y >>= \fm -> dateStringToDateTime currentYear <$> date fm) pairs
        fmap sorter readerIntPostPageMeta

  createIndexModel' postPageMetaPairs = do
    MainPhlogIndex $ do
      currentYear <- ask
      let 
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
  -- | Sort the hashmap values by date.
  sortIndexModel (MainTagIndex mainTagIndexMap) =
    let defaultYear = 2021
        sorter = sortOn (\y -> dateStringToDateTime defaultYear <$> date (snd y))
    in MainTagIndex $ HashMap.map sorter mainTagIndexMap

  -- NOTE: due to PhlogIndex SpecificTagIndex being similar, this results in much overlap/recalculation...
  createIndexModel' (PostPageMeta postPageMetaPairs) =
    MainTagIndex $ HashMap.map (take postsPerTag) $ HashMap.fromListWith (++) result
   where
    -- TODO/FIXME: make into config value... will have to use reader?
    postsPerTag :: Int
    postsPerTag = 3

    result :: [ (Tag, [(FilePath, FrontMatter)]) ]
    result =
      [(tag, take postsPerTag [(filePath, fm)]) | (filePath, Just fm) <- postPageMetaPairs, tag <- tags fm]-- FIXME: what if no tags? that should error right and be just fine?

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


-- FIXME: this no longer makes sense the way it compiles results.
-- this one could be more general like PostPageMetaCriterion for cats and authors etc
--newtype PostPageMetaTag = PostPageMetaTag (Tag, PostPageMeta)
newtype SpecificTagIndex = SpecificTagIndex (Tag, [ (FilePath, FrontMatter) ] )

instance PhlogIndex (PostPageMetaGroupPair Tag) SpecificTagIndex where
  sortIndexModel (SpecificTagIndex (tag, filePathFrontMatterPairs)) =
    let year = 2021
    in SpecificTagIndex $ (tag, sortOn (\y -> date (snd y) >>= pure . dateStringToDateTime year) filePathFrontMatterPairs)

  -- FIXME: is all I need to do is sort postPageMetaPairs?
  createIndexModel' (PostPageMetaGroupPair (_, tag, postPageMetaPairs)) =
    -- FIXME
    SpecificTagIndex (tag, postPageMetaPairs)

  renderIndexGophermap (SpecificTagIndex (tag, specificTagIndexes)) = do
    configParser <- getConfig
    tagIndexPath <- getConfigValue configParser "phlog" "tagPath"
    buildPath <- getConfigValue configParser "general" "buildPath"
    let outputPath = buildPath ++ "/" ++ tagIndexPath ++ "/" ++ (T.unpack tag) ++ "/.gophermap"
        outputDirectoryPath = takeDirectory outputPath
    createDirectoryIfMissing True outputDirectoryPath
    writeFile outputPath (tagIndexContents tag)
   where
    -- FIXME: no need for second argument Tag
    tagIndexContents :: Tag -> String
    tagIndexContents _ =
      -- fromjust bad! FIXME
      (T.unpack tag) ++ "\n\n" ++ (intercalate "\n" $ map (makeLocalLink . (fst &&& Just . snd)) $ specificTagIndexes)


newtype PostPageMetaGroupPair a = PostPageMetaGroupPair (String, a, [(FilePath, FrontMatter)]) -- this is what get accepted by the thingy builder
newtype PostPageMetaGroup a = PostPageMetaGroup (String, HashMap.HashMap a [(FilePath, FrontMatter)]) deriving (Show)


getPostPageMetaGroupPair :: (Eq a, Hashable a) => PostPageMetaGroup a -> a -> PostPageMetaGroupPair a
getPostPageMetaGroupPair (PostPageMetaGroup (label, hashMap)) key =
  -- FIXME: fromjust
  PostPageMetaGroupPair (label, key, fromJust $ HashMap.lookup key hashMap)

-- | Group posts together by some property of the FrontMatter. Weeds out posts
-- without FrontMatter.
--
-- If you're not getting a value that is a list you can use Identity as the return
-- value. 
--
--
-- >>> (frontMatterHashMapGroup (PostPageMeta filePathFrontMatter) ("title", Identity . title) :: (String, HashMap.HashMap (Maybe T.Text) [(FilePath, FrontMatter)]))
-- >>> (frontMatterHashMapGroup (PostPageMeta filePathFrontMatter) ("tag", tags) :: (String, HashMap.HashMap Tag [(FilePath, FrontMatter)]))
frontMatterHashMapGroup
  :: (Eq a, Hashable a, Foldable f)
  => PostPageMeta
  -> (String, FrontMatter -> f a)
  -> PostPageMetaGroup a
frontMatterHashMapGroup (PostPageMeta postPageMetaPairs) (groupName, groupFunction) =
  PostPageMetaGroup $ (groupName, HashMap.fromListWith (++) result)
 where
  result =
    [ (group, [(filePath, fm)]) | (filePath, Just fm) <- postPageMetaPairs, group <- (toList $ groupFunction fm)]

-- FIXME: define Post as (FilePath, Maybe FrontMatter)?

-- FIXME: should i have a function that will create a separation of all the tags into their respective groups so it generates efficiently? this could also be used by the main tag index
-- FIXME: need to get all tags and then traverse or something
-- | Render the tag indexes from a collection of file paths and their
-- associated `FrontMatter`, which contains the tags for that file.
--
-- The tags are written out to the supplied `FilePath`.
renderTagIndexes :: [(FilePath, Maybe FrontMatter)] ->  IO ()
renderTagIndexes filePathFrontMatter = do
  -- FIXME: currently only doing tag "foo"
  --_ <- error . show $ (frontMatterHashMapGroup (PostPageMeta filePathFrontMatter) ("title", Identity . title) :: PostPageMetaGroup (Maybe T.Text))
  --_ <- error . show $ (frontMatterHashMapGroup (PostPageMeta filePathFrontMatter) ("tag", tags) :: PostPageMetaGroup Tag)

  let ppmg@(PostPageMetaGroup (_, hashMap)) = (frontMatterHashMapGroup (PostPageMeta filePathFrontMatter) ("tag", tags) :: PostPageMetaGroup Tag)
      tagsFound = HashMap.keys hashMap
  traverse_ (\x -> renderIndexGophermap (createIndexModel $ getPostPageMetaGroupPair ppmg x :: SpecificTagIndex)) tagsFound

  let mainTagIndex = createIndexModel (PostPageMeta filePathFrontMatter) :: MainTagIndex
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
